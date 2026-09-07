use std::collections::{HashMap, HashSet};

use oxc::{
    allocator::{Allocator, ArenaVec},
    ast::{
        ast::{Expression, Program, Statement, WithStatement},
        builder::AstBuilder,
    },
    ast_visit::{Visit, VisitMut},
    diagnostics::OxcDiagnostic,
    parser::Parser,
    semantic::Scoping,
    span::{SourceType, Span, SPAN},
    str::Ident,
};
use rand::{distr::Alphanumeric, rngs::StdRng, Rng, SeedableRng};

use crate::{
    analyze, ObfuscationError, Options, Pass, SkipReason, SkippedRegion, TransformationReport,
};

/// Basic blocks and resolved local transfers.
mod cfg;
/// Encoded control-flow flattening.
mod control_flow;
/// Pure unreachable branch insertion.
mod dead_code;
/// Exact int32 numeric masking.
mod numeric;
/// Semantic local binding renaming.
mod renaming;
/// String expression encoding.
mod string;

/// Arena, randomness, and reserved names shared by all passes in one invocation.
struct Context<'a> {
    /// Arena used by generated AST nodes.
    ast: AstBuilder<'a>,
    /// The run's single random stream, optionally seeded by the caller.
    rng: StdRng,
    /// Input bindings, unresolved globals, and previously generated names.
    names: HashSet<String>,
    /// Original string spans mapped to their owning block's state.
    string_contexts: HashMap<Span, control_flow::State<'a>>,
    /// Coverage evidence and conservative skip decisions.
    report: TransformationReport,
    /// Internal helper parser errors, propagated through the public error type.
    diagnostics: Vec<OxcDiagnostic>,
    /// Arena for parsing generated helper snippets.
    allocator: &'a Allocator,
}

impl<'a> Context<'a> {
    /// Record why a source candidate was retained.
    fn skip(&mut self, span: Span, reason: SkipReason) {
        self.report.skipped.push(SkippedRegion {
            start: span.start,
            end: span.end,
            reason,
        });
    }

    /// Parse a generated expression; diagnostics prevent any malformed output escaping.
    fn expression(&mut self, source: &str) -> Expression<'a> {
        let source = self.allocator.alloc_str(source);
        match Parser::new(self.allocator, source, SourceType::cjs()).parse_expression() {
            Ok(mut expression) => {
                GeneratedSpans.visit_expression(&mut expression);
                expression
            }
            Err(diagnostics) => {
                self.diagnostics.extend(diagnostics);
                Expression::new_null_literal(SPAN, &self.ast)
            }
        }
    }

    /// Parse helpers only, never printed or reconstructed user AST.
    fn statements(&mut self, source: &str) -> ArenaVec<'a, Statement<'a>> {
        let source = self.allocator.alloc_str(source);
        let mut parsed = Parser::new(self.allocator, source, SourceType::cjs()).parse();
        self.diagnostics.extend(parsed.diagnostics);
        GeneratedSpans.visit_program(&mut parsed.program);
        parsed.program.body
    }

    /// Allocate a random name that cannot capture an input binding or global.
    fn fresh_name(&mut self) -> Ident<'a> {
        loop {
            let suffix: String = (&mut self.rng)
                .sample_iter(Alphanumeric)
                .take(12)
                .map(char::from)
                .collect();
            let name = format!("_{suffix}");
            if self.names.insert(name.clone()) {
                return Ident::from_str_in(&name, &self.ast);
            }
        }
    }
}

/// Helper snippets have no offsets in the original program's source text.
struct GeneratedSpans;

impl VisitMut<'_> for GeneratedSpans {
    fn visit_span(&mut self, span: &mut Span) {
        *span = SPAN;
    }
}

/// Detect with statements without allocating names or rebuilding semantics.
#[derive(Default)]
struct DynamicScope {
    /// A with statement can change lexical identifier lookup at runtime.
    has_with: bool,
}

impl<'a> Visit<'a> for DynamicScope {
    fn visit_with_statement(&mut self, _: &WithStatement<'a>) {
        self.has_with = true;
    }
}

/// Run each enabled pass once, in dependency order, reusing initial scope data.
///
/// # Errors
/// Returns diagnostics if structural transformations produce an invalid AST.
#[expect(
    clippy::redundant_pub_crate,
    reason = "pipeline is visible only to its parent library module"
)]
pub(super) fn run<'a>(
    program: &mut Program<'a>,
    allocator: &'a Allocator,
    mut scoping: Scoping,
    options: &Options,
) -> Result<TransformationReport, ObfuscationError> {
    if options.passes.is_empty() {
        return Ok(TransformationReport::default());
    }
    let mut dynamic_scope = DynamicScope::default();
    dynamic_scope.visit_program(program);
    let names: HashSet<String> = scoping
        .symbol_names()
        .map(str::to_owned)
        .chain(
            scoping
                .root_unresolved_references()
                .keys()
                .map(|name| name.as_str().to_owned()),
        )
        .collect();
    let dynamic = dynamic_scope.has_with || names.contains("eval");
    let mut context = Context {
        ast: AstBuilder::new(allocator),
        rng: options
            .seed
            .map_or_else(StdRng::from_os_rng, StdRng::seed_from_u64),
        names,
        string_contexts: HashMap::new(),
        report: TransformationReport::default(),
        diagnostics: vec![],
        allocator,
    };
    let enabled = |pass| options.passes.contains(&pass);
    if dynamic {
        context.skip(program.span, SkipReason::DynamicScope);
    }
    let structural = !dynamic
        && (enabled(Pass::ControlFlow) || enabled(Pass::DeadCode) || enabled(Pass::String));
    if !dynamic && enabled(Pass::ControlFlow) {
        control_flow::ControlFlow::new(&mut context, &scoping, options).visit_program(program);
    }
    if !dynamic && enabled(Pass::DeadCode) {
        dead_code::DeadCode::new(&mut context).visit_program(program);
    }
    if enabled(Pass::Numeric) {
        numeric::Numeric::new(&mut context).visit_program(program);
    }
    if !dynamic && enabled(Pass::String) && options.max_string_pool_entries > 0 {
        string::Strings::new(&mut context, options.max_string_pool_entries).visit_program(program);
    }
    if !context.diagnostics.is_empty() {
        return Err(ObfuscationError {
            diagnostics: context.diagnostics,
        });
    }
    if structural {
        // Generated declarations need fresh symbol/reference IDs. Also validate
        // structural output when renaming is disabled.
        scoping = analyze(program)?;
    }
    if !dynamic && enabled(Pass::Renaming) {
        renaming::rename(program, &scoping, &mut context);
    }
    Ok(context.report)
}

/// Create an exact int32 number expression.
fn number<'a>(
    ast: &AstBuilder<'a>,
    span: oxc::span::Span,
    value: i32,
) -> oxc::ast::ast::Expression<'a> {
    oxc::ast::ast::Expression::new_numeric_literal(
        span,
        f64::from(value),
        None,
        oxc::ast::ast::NumberBase::Decimal,
        ast,
    )
}
