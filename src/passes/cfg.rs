use oxc::{
    allocator::{ArenaVec, CloneIn},
    ast::ast::{Expression, ForStatementInit, Statement, VariableDeclarationKind},
    semantic::Scoping,
    str::Ident,
};

use super::Context;
use crate::SkipReason;

/// An index into one region's block vector.
pub(super) type BlockId = usize;

/// Explicit transfers keep user expressions separate from generated state.
pub(super) enum Terminator<'a> {
    /// Unconditional local edge.
    Jump(BlockId),
    /// Evaluate the original condition exactly once.
    Branch(Expression<'a>, BlockId, BlockId),
    /// Return from the original function.
    Return(Option<Expression<'a>>),
    /// Throw into the original enclosing exception boundary.
    Throw(Expression<'a>),
    /// Resume the surrounding structured statement list.
    Exit,
}

/// Original statements followed by exactly one explicit transfer.
pub(super) struct Block<'a> {
    /// Statements retain their declaration initialization order.
    pub(super) statements: ArenaVec<'a, Statement<'a>>,
    /// The block's outgoing control transfer.
    pub(super) terminator: Terminator<'a>,
}

/// A resolved loop or label target, scoped to CFG construction.
struct Target<'a> {
    /// Label name, if any.
    label: Option<Ident<'a>>,
    /// Target for break.
    exit: BlockId,
    /// Target for continue, for loops only.
    repeat: Option<BlockId>,
}

/// A bounded control-flow region; block zero is its exit.
pub(super) struct Graph<'a> {
    /// Blocks in construction order, shuffled only at emission.
    pub(super) blocks: Vec<Block<'a>>,
    /// First block to execute.
    pub(super) entry: BlockId,
    /// Active loop and label targets during lowering.
    targets: Vec<Target<'a>>,
    /// Limit checked during construction, before allocating more blocks.
    limit: usize,
}

impl<'a> Graph<'a> {
    /// Build speculatively from arena clones so an ineligible region stays intact.
    ///
    /// # Errors
    /// Returns the first unsupported boundary or the configured block limit.
    pub(super) fn build(
        statements: &[Statement<'a>],
        context: &Context<'a>,
        scoping: &Scoping,
        limit: usize,
    ) -> Result<Self, SkipReason> {
        let mut graph = Self {
            blocks: vec![],
            entry: 0,
            targets: vec![],
            limit,
        };
        graph.push(context, None, Terminator::Exit)?;
        for statement in statements.iter().rev() {
            graph.entry = graph.lower(
                statement.clone_in_with_semantic_ids(context.allocator),
                graph.entry,
                context,
                scoping,
            )?;
        }
        Ok(graph)
    }

    /// Allocate a block without allowing an unbounded dispatcher.
    ///
    /// # Errors
    /// Returns a size skip when the configured block limit is reached.
    fn push(
        &mut self,
        context: &Context<'a>,
        statement: Option<Statement<'a>>,
        terminator: Terminator<'a>,
    ) -> Result<BlockId, SkipReason> {
        if self.blocks.len() >= self.limit {
            return Err(SkipReason::DispatcherLimit);
        }
        let id = self.blocks.len();
        self.blocks.push(Block {
            statements: ArenaVec::from_iter_in(statement, &context.ast),
            terminator,
        });
        Ok(id)
    }

    /// Find a label or the innermost applicable loop without crossing the region.
    ///
    /// # Errors
    /// Returns a transfer skip when its target is outside this region.
    fn target(&self, label: Option<Ident<'a>>, continuing: bool) -> Result<BlockId, SkipReason> {
        self.targets
            .iter()
            .rev()
            .find(|target| {
                if label.is_some() {
                    target.label == label
                } else {
                    target.repeat.is_some()
                }
            })
            .and_then(|target| {
                if continuing {
                    target.repeat
                } else {
                    Some(target.exit)
                }
            })
            .ok_or(SkipReason::ExternalTransfer)
    }

    /// Lower one statement backwards from its continuation.
    ///
    /// # Errors
    /// Returns unsupported scope, transfer or size boundaries.
    #[expect(
        clippy::wildcard_enum_match_arm,
        reason = "unknown AST statement forms must conservatively retain their structure"
    )]
    fn lower(
        &mut self,
        statement: Statement<'a>,
        next: BlockId,
        context: &Context<'a>,
        scoping: &Scoping,
    ) -> Result<BlockId, SkipReason> {
        match statement {
            Statement::EmptyStatement(_) => Ok(next),
            Statement::ExpressionStatement(_) => {
                self.push(context, Some(statement), Terminator::Jump(next))
            }
            Statement::VariableDeclaration(declaration) => {
                if declaration.kind != VariableDeclarationKind::Var {
                    return Err(SkipReason::LexicalScope);
                }
                self.push(
                    context,
                    Some(Statement::VariableDeclaration(declaration)),
                    Terminator::Jump(next),
                )
            }
            Statement::FunctionDeclaration(_) | Statement::ClassDeclaration(_) => {
                Err(SkipReason::LexicalScope)
            }
            Statement::BlockStatement(block) => {
                self.lower_block(block.unbox(), next, context, scoping)
            }
            Statement::IfStatement(branch) => {
                let branch = branch.unbox();
                let yes = self.lower(branch.consequent, next, context, scoping)?;
                let no = branch.alternate.map_or(Ok(next), |alternate| {
                    self.lower(alternate, next, context, scoping)
                })?;
                self.push(context, None, Terminator::Branch(branch.test, yes, no))
            }
            Statement::WhileStatement(_)
            | Statement::DoWhileStatement(_)
            | Statement::ForStatement(_) => self.lower_loop(statement, next, &[], context, scoping),
            Statement::LabeledStatement(label) => {
                self.lower_label(label.unbox(), next, context, scoping)
            }
            Statement::BreakStatement(transfer) => {
                self.target(transfer.label.as_ref().map(|label| label.name), false)
            }
            Statement::ContinueStatement(transfer) => {
                self.target(transfer.label.as_ref().map(|label| label.name), true)
            }
            Statement::ReturnStatement(transfer) => {
                self.push(context, None, Terminator::Return(transfer.unbox().argument))
            }
            Statement::ThrowStatement(transfer) => {
                self.push(context, None, Terminator::Throw(transfer.unbox().argument))
            }
            _ => Err(SkipReason::UnsupportedStatement),
        }
    }

    /// Retain block bindings while lowering binding-free statement lists.
    ///
    /// # Errors
    /// Returns a lexical scope, unsupported child or size boundary.
    fn lower_block(
        &mut self,
        block: oxc::ast::ast::BlockStatement<'a>,
        next: BlockId,
        context: &Context<'a>,
        scoping: &Scoping,
    ) -> Result<BlockId, SkipReason> {
        // Semantic binding data also catches scope-bearing declarations that
        // must not be moved into the switch's shared lexical environment.
        if block
            .scope_id
            .get()
            .is_some_and(|scope| !scoping.get_bindings(scope).is_empty())
        {
            return Err(SkipReason::LexicalScope);
        }
        let mut entry = next;
        for child in block.body.into_iter().rev() {
            entry = self.lower(child, entry, context, scoping)?;
        }
        Ok(entry)
    }

    /// Resolve stacked labels, including multiple labels on the same loop.
    ///
    /// # Errors
    /// Returns unsupported local transfers, child statements or size boundaries.
    fn lower_label(
        &mut self,
        label: oxc::ast::ast::LabeledStatement<'a>,
        next: BlockId,
        context: &Context<'a>,
        scoping: &Scoping,
    ) -> Result<BlockId, SkipReason> {
        let mut labels = vec![label.label.name];
        let mut body = label.body;
        while let Statement::LabeledStatement(inner) = body {
            labels.push(inner.label.name);
            body = inner.unbox().body;
        }
        if matches!(
            body,
            Statement::WhileStatement(_)
                | Statement::DoWhileStatement(_)
                | Statement::ForStatement(_)
        ) {
            return self.lower_loop(body, next, &labels, context, scoping);
        }
        let depth = self.targets.len();
        self.targets.extend(labels.into_iter().map(|label| Target {
            label: Some(label),
            exit: next,
            repeat: None,
        }));
        let entry = self.lower(body, next, context, scoping)?;
        self.targets.truncate(depth);
        Ok(entry)
    }

    /// Resolve loop test, update, and labeled transfers before emitting its body.
    ///
    /// # Errors
    /// Returns unsupported scope, transfer or size boundaries in the loop.
    #[expect(
        clippy::wildcard_enum_match_arm,
        reason = "only the three explicitly supported loop forms may be lowered"
    )]
    #[allow(
        clippy::too_many_arguments,
        reason = "strict and normal profiles have different argument thresholds; lowering needs the loop, destination, labels, arena and bindings"
    )]
    fn lower_loop(
        &mut self,
        statement: Statement<'a>,
        next: BlockId,
        labels: &[Ident<'a>],
        context: &Context<'a>,
        scoping: &Scoping,
    ) -> Result<BlockId, SkipReason> {
        let (init, test, update, body, do_first) = match statement {
            Statement::WhileStatement(loop_) => {
                let loop_ = loop_.unbox();
                (None, Some(loop_.test), None, loop_.body, false)
            }
            Statement::DoWhileStatement(loop_) => {
                let loop_ = loop_.unbox();
                (None, Some(loop_.test), None, loop_.body, true)
            }
            Statement::ForStatement(loop_) => {
                let loop_ = loop_.unbox();
                let init = match loop_.init {
                    Some(ForStatementInit::VariableDeclaration(declaration)) => {
                        if declaration.kind != VariableDeclarationKind::Var {
                            return Err(SkipReason::LexicalScope);
                        }
                        Some(Statement::VariableDeclaration(declaration))
                    }
                    Some(expression) => Some(Statement::new_expression_statement(
                        oxc::span::SPAN,
                        expression.into_expression(),
                        &context.ast,
                    )),
                    None => None,
                };
                (init, loop_.test, loop_.update, loop_.body, false)
            }
            _ => return Err(SkipReason::UnsupportedStatement),
        };
        let test_id = self.push(context, None, Terminator::Exit)?;
        let repeat = if let Some(update) = update {
            self.push(
                context,
                Some(Statement::new_expression_statement(
                    oxc::span::SPAN,
                    update,
                    &context.ast,
                )),
                Terminator::Jump(test_id),
            )?
        } else {
            test_id
        };
        let depth = self.targets.len();
        self.targets.push(Target {
            label: None,
            exit: next,
            repeat: Some(repeat),
        });
        self.targets.extend(labels.iter().map(|label| Target {
            label: Some(*label),
            exit: next,
            repeat: Some(repeat),
        }));
        let body_id = self.lower(body, repeat, context, scoping)?;
        self.targets.truncate(depth);
        if let Some(block) = self.blocks.get_mut(test_id) {
            block.terminator = test.map_or(Terminator::Jump(body_id), |test| {
                Terminator::Branch(test, body_id, next)
            });
        }
        let entry = if do_first { body_id } else { test_id };
        init.map_or(Ok(entry), |init| self.lower(init, entry, context, scoping))
    }
}
