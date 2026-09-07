//! Command-line JavaScript obfuscation with file and standard-stream support.

use std::{
    fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, ValueEnum};
use hide_my_js::{obfuscate_with_report, Options, Pass, SourceKind};
use {oxc as _, rand as _};

/// Obfuscate JavaScript from a file or standard input.
#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    /// Input JavaScript file; omit or use - to read UTF-8 from stdin.
    #[arg(value_name = "INPUT")]
    input: Option<PathBuf>,
    /// Output file; omit or use - to write JavaScript to stdout.
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,
    /// JavaScript parsing mode (file extensions do not select a mode).
    #[arg(long, value_enum, default_value = "common-js")]
    source_kind: SourceMode,
    /// Enable only these passes (comma-separated or repeated); default: all.
    #[arg(long, value_enum, value_delimiter = ',', value_name = "PASS")]
    passes: Vec<SelectedPass>,
    /// Only validate and format the input, without obfuscation passes.
    #[arg(long, conflicts_with = "passes")]
    no_passes: bool,
    /// Seed repeatable output for the same input and options.
    #[arg(long)]
    seed: Option<u64>,
    /// Percentage of eligible control-flow regions to flatten (0-100).
    #[arg(long, default_value_t = Options::default().control_flow_coverage, value_parser = clap::value_parser!(u8).range(0..=100))]
    control_flow_coverage: u8,
    /// Maximum blocks per dispatcher, including the exit; below 3 disables flattening.
    #[arg(long, default_value_t = Options::default().max_dispatcher_blocks)]
    max_dispatcher_blocks: usize,
    /// Maximum string occurrences per pool; 0 disables pooling.
    #[arg(long, default_value_t = Options::default().max_string_pool_entries)]
    max_string_pool_entries: usize,
    /// Print transformation counts to stderr.
    #[arg(long)]
    report: bool,
}

/// CLI spellings are kept separate from the library's public types.
#[derive(Debug, Clone, Copy, ValueEnum)]
enum SourceMode {
    /// Standalone JavaScript script.
    Script,
    /// `CommonJS` script, allowing top-level return.
    CommonJs,
    /// ECMAScript module, allowing imports, exports, and top-level await.
    Module,
}

impl From<SourceMode> for SourceKind {
    fn from(mode: SourceMode) -> Self {
        match mode {
            SourceMode::Script => Self::Script,
            SourceMode::CommonJs => Self::CommonJs,
            SourceMode::Module => Self::Module,
        }
    }
}

/// Names accepted by the pass-selection argument.
#[derive(Debug, Clone, Copy, ValueEnum)]
enum SelectedPass {
    /// Flatten eligible branches and loops into encoded dispatchers.
    ControlFlow,
    /// Insert unreachable branches.
    DeadCode,
    /// Mask int32 numeric expressions.
    Numeric,
    /// Encode strings in scoped pools.
    String,
    /// Rename local bindings.
    Renaming,
}

impl From<SelectedPass> for Pass {
    fn from(pass: SelectedPass) -> Self {
        match pass {
            SelectedPass::ControlFlow => Self::ControlFlow,
            SelectedPass::DeadCode => Self::DeadCode,
            SelectedPass::Numeric => Self::Numeric,
            SelectedPass::String => Self::String,
            SelectedPass::Renaming => Self::Renaming,
        }
    }
}

impl Cli {
    /// Apply CLI choices without changing the library's canonical pass ordering.
    fn options(&self) -> Options {
        let passes = if self.no_passes {
            vec![]
        } else if self.passes.is_empty() {
            Pass::ALL.to_vec()
        } else {
            self.passes.iter().copied().map(Pass::from).collect()
        };
        Options {
            source_kind: self.source_kind.into(),
            passes,
            seed: self.seed,
            control_flow_coverage: self.control_flow_coverage,
            max_dispatcher_blocks: self.max_dispatcher_blocks,
            max_string_pool_entries: self.max_string_pool_entries,
        }
    }

    /// Read and validate completely before opening any output file.
    ///
    /// # Errors
    /// Returns contextual input, transformation, or output errors for stderr.
    fn run(&self) -> Result<(), String> {
        let input = file_path(self.input.as_deref());
        let label = input.map_or_else(|| "stdin".to_owned(), |path| path.display().to_string());
        let source = if let Some(path) = input {
            fs::read_to_string(path).map_err(|error| format!("cannot read {label}: {error}"))?
        } else {
            let mut source = String::new();
            io::stdin()
                .lock()
                .read_to_string(&mut source)
                .map_err(|error| format!("cannot read stdin: {error}"))?;
            source
        };
        let output = obfuscate_with_report(&source, &self.options())
            .map_err(|error| format!("{label}: {error}"))?;
        if let Some(path) = file_path(self.output.as_deref()) {
            fs::write(path, &output.code)
                .map_err(|error| format!("cannot write {}: {error}", path.display()))?;
        } else if let Err(error) = io::stdout().lock().write_all(output.code.as_bytes()) {
            if error.kind() == io::ErrorKind::BrokenPipe {
                return Ok(());
            }
            return Err(format!("cannot write stdout: {error}"));
        }
        if self.report {
            let report = output.report;
            writeln!(io::stderr().lock(),
                "dispatchers: {}, blocks: {}, strings: {}, contextual strings: {}, pools: {}, skipped candidates: {}",
                report.flattened_regions, report.basic_blocks, report.encoded_strings,
                report.contextual_strings, report.string_pools, report.skipped.len(),
            ).map_err(|error| format!("cannot write report: {error}"))?;
        }
        Ok(())
    }
}

/// Interpret an absent path or a literal dash as a standard stream.
fn file_path(path: Option<&Path>) -> Option<&Path> {
    path.filter(|path| *path != Path::new("-"))
}

/// Keep diagnostics on stderr and report operational failures through the exit code.
#[expect(
    clippy::let_underscore_must_use,
    reason = "there is no fallback stream when printing a final stderr diagnostic fails"
)]
fn main() -> ExitCode {
    match Cli::parse().run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            let _ = writeln!(io::stderr().lock(), "hide-my-js: {error}");
            ExitCode::FAILURE
        }
    }
}
