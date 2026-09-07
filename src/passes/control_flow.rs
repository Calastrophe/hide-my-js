use std::collections::HashSet;

use oxc::{
    allocator::ArenaVec,
    ast::ast::{
        ArrowFunctionExpression, BlockStatement, Expression, Function, FunctionBody, Statement,
        StringLiteral, SwitchCase,
    },
    ast_visit::{walk, walk_mut, Visit, VisitMut},
    semantic::{ScopeFlags, Scoping},
    span::{GetSpan, SPAN},
    str::Ident,
};
use rand::{seq::SliceRandom, Rng};

use super::{
    cfg::{Graph, Terminator},
    number, Context,
};
use crate::{Options, SkipReason};

/// A block's immutable decoding context: `left ^ right == identity`.
#[derive(Clone, Copy)]
pub(super) struct State<'a> {
    /// First generated state binding.
    pub(super) left: Ident<'a>,
    /// Second generated state binding.
    pub(super) right: Ident<'a>,
    /// Unique logical block identity.
    pub(super) identity: i32,
    /// Right-hand value; left-hand value is identity XOR mask.
    mask: i32,
}

impl State<'_> {
    /// Expression yielding the block identity at every execution of this block.
    pub(super) fn invariant(self) -> String {
        format!("({}^{})", self.left, self.right)
    }

    /// Both assignments consume old state and establish the destination's pair.
    fn transition(self, next: Self) -> String {
        format!(
            "{}={}^{};{}={}^{};",
            self.left,
            self.invariant(),
            self.identity ^ next.identity ^ next.mask,
            self.right,
            self.right,
            self.mask ^ next.mask
        )
    }
}

/// Lower eligible local regions while retaining structured scope boundaries.
pub(super) struct ControlFlow<'ctx, 'a> {
    /// Run state and arena.
    context: &'ctx mut Context<'a>,
    /// Original semantic binding information.
    scoping: &'ctx Scoping,
    /// Coverage and dispatcher size settings.
    options: &'ctx Options,
    /// True only while visiting an ordinary synchronous function.
    eligible_function: bool,
}

impl<'ctx, 'a> ControlFlow<'ctx, 'a> {
    /// Borrow configuration and original semantics for this pass.
    pub(super) const fn new(
        context: &'ctx mut Context<'a>,
        scoping: &'ctx Scoping,
        options: &'ctx Options,
    ) -> Self {
        Self {
            context,
            scoping,
            options,
            eligible_function: false,
        }
    }

    /// Randomize identities and layouts, keeping all state local to this invocation.
    fn emit(&mut self, graph: Graph<'a>) -> Statement<'a> {
        let left = self.context.fresh_name();
        let right = self.context.fresh_name();
        let mut used = HashSet::new();
        let states: Vec<_> = graph
            .blocks
            .iter()
            .map(|_| {
                let mut identity = self.context.rng.random::<i32>();
                while !used.insert(identity) {
                    identity = self.context.rng.random();
                }
                State {
                    left,
                    right,
                    identity,
                    mask: self.context.rng.random(),
                }
            })
            .collect();
        // Every graph has an exit and a valid entry; zip below preserves that index relation.
        let Some(initial) = states.get(graph.entry).copied() else {
            return Statement::new_empty_statement(SPAN, &self.context.ast);
        };
        let Some(exit) = states.first().copied() else {
            return Statement::new_empty_statement(SPAN, &self.context.ast);
        };
        let mut cases = ArenaVec::new_in(&self.context.ast);
        self.context.report.flattened_regions += 1;
        self.context.report.basic_blocks += graph.blocks.len();
        for (block, state) in graph.blocks.into_iter().zip(states.iter().copied()) {
            if let Some(case) = self.emit_case(block, state, &states) {
                cases.push(case);
            }
        }
        cases.shuffle(&mut self.context.rng);
        let declarations = format!(
            "let {left}={}, {right}={};",
            initial.identity ^ initial.mask,
            initial.mask
        );
        let mut body = self.context.statements(&declarations);
        let test = self
            .context
            .expression(&format!("{}!=={}", initial.invariant(), exit.identity));
        let discriminant = self.context.expression(&initial.invariant());
        let switch = Statement::new_switch_statement(SPAN, discriminant, cases, &self.context.ast);
        let loop_ = if self.context.rng.random::<bool>() {
            Statement::new_while_statement(SPAN, test, switch, &self.context.ast)
        } else {
            Statement::new_for_statement(SPAN, None, Some(test), None, switch, &self.context.ast)
        };
        body.push(loop_);
        Statement::new_block_statement(SPAN, body, &self.context.ast)
    }

    /// Attach original string ownership and emit a block's explicit transfer.
    fn emit_case(
        &mut self,
        mut block: super::cfg::Block<'a>,
        state: State<'a>,
        states: &[State<'a>],
    ) -> Option<SwitchCase<'a>> {
        // Record original literal ownership before constructing generated AST.
        let mut owner = StringOwner {
            context: self.context,
            state,
        };
        owner.visit_statements(&block.statements);
        match &block.terminator {
            Terminator::Branch(test, _, _) | Terminator::Throw(test) => {
                owner.visit_expression(test);
            }
            Terminator::Return(Some(value)) => owner.visit_expression(value),
            Terminator::Jump(_) | Terminator::Return(None) | Terminator::Exit => {}
        }
        match block.terminator {
            Terminator::Jump(next) => {
                if let Some(next) = states.get(next) {
                    block
                        .statements
                        .extend(self.context.statements(&state.transition(*next)));
                }
            }
            Terminator::Branch(test, yes, no) => {
                if let (Some(yes), Some(no)) = (states.get(yes), states.get(no)) {
                    let consequent = self.context.statements(&state.transition(*yes));
                    let alternate = self.context.statements(&state.transition(*no));
                    block.statements.push(Statement::new_if_statement(
                        SPAN,
                        test,
                        Statement::new_block_statement(SPAN, consequent, &self.context.ast),
                        Some(Statement::new_block_statement(
                            SPAN,
                            alternate,
                            &self.context.ast,
                        )),
                        &self.context.ast,
                    ));
                }
            }
            Terminator::Return(value) => block.statements.push(Statement::new_return_statement(
                SPAN,
                value,
                &self.context.ast,
            )),
            Terminator::Throw(value) => block.statements.push(Statement::new_throw_statement(
                SPAN,
                value,
                &self.context.ast,
            )),
            Terminator::Exit => return None,
        }
        block.statements.push(Statement::new_break_statement(
            SPAN,
            None,
            &self.context.ast,
        ));
        Some(SwitchCase::new(
            SPAN,
            Some(number(&self.context.ast, SPAN, state.identity)),
            block.statements,
            &self.context.ast,
        ))
    }

    /// Emit one selected run, leaving tiny and coverage-excluded runs intact.
    fn flush(
        &mut self,
        run: &mut ArenaVec<'a, Statement<'a>>,
        output: &mut ArenaVec<'a, Statement<'a>>,
    ) {
        if run.is_empty() {
            return;
        }
        let graph = Graph::build(
            run,
            self.context,
            self.scoping,
            self.options.max_dispatcher_blocks,
        );
        match graph {
            Ok(graph) if graph.blocks.len() >= 3 => {
                if self.context.rng.random_range(0..100u8)
                    < self.options.control_flow_coverage.min(100)
                {
                    output.push(self.emit(graph));
                    run.clear();
                } else {
                    if let Some(first) = run.first() {
                        self.context.skip(first.span(), SkipReason::Coverage);
                    }
                    output.append(run);
                }
            }
            Ok(_) => output.append(run),
            Err(reason) => {
                if let Some(first) = run.first() {
                    self.context.skip(first.span(), reason);
                }
                output.append(run);
            }
        }
    }

    /// Partition on lexical scopes, unsupported statements, external transfers and size.
    fn flatten_statements(&mut self, statements: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(statements, ArenaVec::new_in(&self.context.ast));
        let mut run = ArenaVec::new_in(&self.context.ast);
        let mut blocks = 1;
        for mut statement in original {
            match Graph::build(
                std::slice::from_ref(&statement),
                self.context,
                self.scoping,
                self.options.max_dispatcher_blocks,
            ) {
                Ok(graph) => {
                    let count = graph.blocks.len() - 1;
                    if blocks + count > self.options.max_dispatcher_blocks {
                        self.flush(&mut run, statements);
                        blocks = 1;
                    }
                    NestedFunctions { flow: self }.visit_statement(&mut statement);
                    run.push(statement);
                    blocks += count;
                }
                Err(reason) => {
                    self.flush(&mut run, statements);
                    blocks = 1;
                    self.context.skip(statement.span(), reason);
                    // Keep the boundary, but still visit safe regions within it.
                    walk_mut::walk_statement(self, &mut statement);
                    statements.push(statement);
                }
            }
        }
        self.flush(&mut run, statements);
    }
}

impl<'a> VisitMut<'a> for ControlFlow<'_, 'a> {
    fn visit_function(&mut self, function: &mut Function<'a>, flags: ScopeFlags) {
        let previous = self.eligible_function;
        self.eligible_function = !function.r#async && !function.generator;
        if !self.eligible_function {
            self.context.skip(function.span, SkipReason::Suspension);
        }
        walk_mut::walk_function(self, function, flags);
        self.eligible_function = previous;
    }

    fn visit_arrow_function_expression(&mut self, arrow: &mut ArrowFunctionExpression<'a>) {
        let previous = self.eligible_function;
        self.eligible_function = !arrow.r#async;
        if arrow.r#async {
            self.context.skip(arrow.span, SkipReason::Suspension);
        }
        walk_mut::walk_arrow_function_expression(self, arrow);
        self.eligible_function = previous;
    }

    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        if self.eligible_function {
            self.flatten_statements(&mut body.statements);
        } else {
            walk_mut::walk_function_body(self, body);
        }
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement<'a>) {
        if self.eligible_function {
            self.flatten_statements(&mut block.body);
        } else {
            walk_mut::walk_block_statement(self, block);
        }
    }
}

/// Find nested functions without prematurely flattening the enclosing region.
struct NestedFunctions<'flow, 'ctx, 'a> {
    /// Delegate function boundaries to the main visitor.
    flow: &'flow mut ControlFlow<'ctx, 'a>,
}

impl<'a> VisitMut<'a> for NestedFunctions<'_, '_, 'a> {
    fn visit_function(&mut self, function: &mut Function<'a>, flags: ScopeFlags) {
        self.flow.visit_function(function, flags);
    }
    fn visit_arrow_function_expression(&mut self, arrow: &mut ArrowFunctionExpression<'a>) {
        self.flow.visit_arrow_function_expression(arrow);
    }
}

/// Attribute only immediately evaluated literals to the current block.
struct StringOwner<'ctx, 'a> {
    /// Run metadata, carried directly to string planning.
    context: &'ctx mut Context<'a>,
    /// State guaranteed at block entry.
    state: State<'a>,
}

impl<'a> Visit<'a> for StringOwner<'_, 'a> {
    fn visit_string_literal(&mut self, literal: &StringLiteral<'a>) {
        self.context
            .string_contexts
            .insert(literal.span, self.state);
    }
    fn visit_function(&mut self, _: &Function<'a>, _: ScopeFlags) {}
    fn visit_arrow_function_expression(&mut self, _: &ArrowFunctionExpression<'a>) {}
    // Classes may defer instance fields; keep the entire class independent.
    fn visit_class(&mut self, _: &oxc::ast::ast::Class<'a>) {}
    fn visit_expression(&mut self, expression: &Expression<'a>) {
        walk::walk_expression(self, expression);
    }
}
