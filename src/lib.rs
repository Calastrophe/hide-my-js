//! JavaScript obfuscation with validated input and independently selectable passes.
//!
//! ```
//! use hide_my_js::{obfuscate, Options};
//!
//! let output = obfuscate("function double(value) { return value * 2; }", &Options::default())?;
//! # Ok::<(), hide_my_js::ObfuscationError>(())
//! ```

// Clap is used by the optional binary in this package.
#[cfg(feature = "cli")]
use clap as _;

/// AST transformations and their shared per-run state.
mod passes;

use std::fmt;

use oxc::{
    allocator::Allocator,
    codegen::Codegen,
    diagnostics::OxcDiagnostic,
    parser::Parser,
    semantic::{Scoping, SemanticBuilder},
    span::SourceType,
};

/// JavaScript parsing mode.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum SourceKind {
    /// A standalone script with browser-style top-level semantics.
    Script,
    /// A script allowing `CommonJS` top-level return statements.
    #[default]
    CommonJs,
    /// An ECMAScript module, including imports, exports, and top-level await.
    Module,
}

impl SourceKind {
    /// Map the public JavaScript modes to Oxc's parser configuration.
    const fn source_type(self) -> SourceType {
        match self {
            Self::Script => SourceType::script(),
            Self::CommonJs => SourceType::cjs(),
            Self::Module => SourceType::mjs(),
        }
    }
}

/// An independently selectable transformation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Pass {
    /// Lower eligible branches and loops into encoded switch dispatchers.
    ControlFlow,
    /// Add pure, unreachable branches to function bodies.
    DeadCode,
    /// Mask int32 numeric expressions with XOR.
    Numeric,
    /// Encode string expressions in scoped UTF-16 pools.
    String,
    /// Rename local bindings using semantic symbol resolution.
    Renaming,
}

impl Pass {
    /// Every pass, in execution order.
    pub const ALL: [Self; 5] = [
        Self::ControlFlow,
        Self::DeadCode,
        Self::Numeric,
        Self::String,
        Self::Renaming,
    ];
}

/// Configuration for one obfuscation run. Defaults enable every pass.
#[derive(Debug, Clone)]
#[expect(
    clippy::exhaustive_structs,
    reason = "configuration intentionally supports struct literals"
)]
pub struct Options {
    /// JavaScript parsing mode; defaults to `CommonJS`.
    pub source_kind: SourceKind,
    /// Passes to enable. Execution order is fixed and duplicates are ignored.
    pub passes: Vec<Pass>,
    /// Repeatable randomness for the same source, options, and library version.
    /// `None` seeds the run from the operating system.
    pub seed: Option<u64>,
    /// Percentage of eligible control-flow regions selected, clamped to 100.
    pub control_flow_coverage: u8,
    /// Maximum basic blocks per dispatcher, including its exit. Below 3 disables flattening.
    pub max_dispatcher_blocks: usize,
    /// Maximum string occurrences in each pool. Zero disables string pooling.
    pub max_string_pool_entries: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            source_kind: SourceKind::default(),
            passes: Pass::ALL.to_vec(),
            seed: None,
            control_flow_coverage: 100,
            max_dispatcher_blocks: 64,
            max_string_pool_entries: 32,
        }
    }
}

/// Parser or semantic diagnostics that prevented obfuscation.
#[derive(Debug)]
pub struct ObfuscationError {
    /// Diagnostics retain Oxc source spans and explanatory labels.
    diagnostics: Vec<OxcDiagnostic>,
}

impl ObfuscationError {
    /// Inspect the diagnostics, including their source spans and labels.
    pub fn diagnostics(&self) -> &[OxcDiagnostic] {
        &self.diagnostics
    }
}

impl fmt::Display for ObfuscationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, diagnostic) in self.diagnostics.iter().enumerate() {
            if index != 0 {
                formatter.write_str("\n")?;
            }
            write!(formatter, "{diagnostic}")?;
        }
        Ok(())
    }
}

impl std::error::Error for ObfuscationError {}

/// Obfuscate JavaScript with the selected passes.
///
/// Input is validated before transformation. Each invocation owns its random
/// generator and reserved names; calls cannot influence one another.
///
/// # Errors
/// Returns parser or semantic diagnostics for invalid JavaScript, including
/// TypeScript and JSX syntax. Semantic failures after structural transformations
/// are returned as well, rather than silently skipping renaming.
pub fn obfuscate(source: &str, options: &Options) -> Result<String, ObfuscationError> {
    obfuscate_with_report(source, options).map(|output| output.code)
}

/// Why an input region retained its structured form.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum SkipReason {
    /// A lexical binding or declaration must retain its scope.
    LexicalScope,
    /// A transfer targets a loop or label outside the candidate region.
    ExternalTransfer,
    /// Exceptions, iteration protocols, or another unsupported statement boundary.
    UnsupportedStatement,
    /// An async or generator function can suspend execution.
    Suspension,
    /// Dynamic identifier lookup prevents safe helper insertion.
    DynamicScope,
    /// The configured dispatcher size would be exceeded.
    DispatcherLimit,
    /// The seeded coverage selection excluded this region.
    Coverage,
    /// Parameter expressions cannot reference helpers in the function body.
    Parameters,
}

/// A skipped candidate and its byte offsets in the original source.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct SkippedRegion {
    /// Inclusive starting byte offset.
    pub start: u32,
    /// Exclusive ending byte offset.
    pub end: u32,
    /// Boundary or configuration responsible for the skip.
    pub reason: SkipReason,
}

/// Transformation counts and conservative eligibility decisions.
#[derive(Debug, Default, Clone)]
#[non_exhaustive]
pub struct TransformationReport {
    /// Number of emitted dispatchers.
    pub flattened_regions: usize,
    /// Total emitted blocks, including dispatcher exits.
    pub basic_blocks: usize,
    /// Number of pooled string occurrences.
    pub encoded_strings: usize,
    /// Pooled occurrences whose keys consume their owning block's state.
    pub contextual_strings: usize,
    /// Number of emitted pool helpers.
    pub string_pools: usize,
    /// Candidates retained because of boundaries or configuration.
    pub skipped: Vec<SkippedRegion>,
}

/// Generated JavaScript together with transformation evidence.
#[derive(Debug)]
#[non_exhaustive]
pub struct ObfuscationOutput {
    /// Formatted generated JavaScript.
    pub code: String,
    /// Coverage and eligibility information.
    pub report: TransformationReport,
}

/// Obfuscate JavaScript and retain coverage and skip information.
///
/// # Errors
/// Returns the same parser and semantic diagnostics as [`obfuscate`].
pub fn obfuscate_with_report(
    source: &str,
    options: &Options,
) -> Result<ObfuscationOutput, ObfuscationError> {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, options.source_kind.source_type()).parse();
    if !parsed.diagnostics.is_empty() {
        return Err(ObfuscationError {
            diagnostics: parsed.diagnostics.into_vec(),
        });
    }
    let mut program = parsed.program;
    let scoping = analyze(&program)?;
    let report = passes::run(&mut program, &allocator, scoping, options)?;
    Ok(ObfuscationOutput {
        code: Codegen::new().build(&program).code,
        report,
    })
}

/// Build owned scope information and propagate semantic validation failures.
///
/// # Errors
/// Returns diagnostics when the AST violates JavaScript semantic rules.
fn analyze(program: &oxc::ast::ast::Program<'_>) -> Result<Scoping, ObfuscationError> {
    let checked = SemanticBuilder::new()
        .with_check_syntax_error(true)
        .build(program);
    if !checked.diagnostics.is_empty() {
        return Err(ObfuscationError {
            diagnostics: checked.diagnostics.into_vec(),
        });
    }
    Ok(checked.semantic.into_scoping())
}
