use hide_my_js::{obfuscate, obfuscate_with_report, ObfuscationOutput, Options};
use std::{
    io::Write,
    process::{Command, Stdio},
};

/// Read generated AST only when a test must establish that a pass actually ran.
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) mod structure;

/// Small reproducible sample of dispatcher layouts, masks, names and pool ordering.
/// Exhaustive pass selection is tested separately from randomization.
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) const SEEDS: [u64; 3] = [0, 1, 2];

/// Transform a fixture through the public library entry point.
///
/// # Panics
/// Fails when a fixture cannot be transformed.
#[expect(
    clippy::expect_used,
    reason = "source fixtures must be valid JavaScript"
)]
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) fn transform(source: &str, options: &Options) -> String {
    obfuscate(source, options).expect("valid JavaScript fixture")
}

/// Execute JavaScript with a timeout so a broken dispatcher fails instead of spinning.
///
/// # Panics
/// Fails on process errors, JavaScript exceptions or assertion failures.
#[expect(
    clippy::expect_used,
    reason = "Node and the fixture's piped I/O must be available"
)]
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) fn execute(code: &str) -> String {
    let mut child = Command::new("node")
        .args(["-e", "const { Worker }=require('node:worker_threads');const worker=new Worker(require('node:fs').readFileSync(0,'utf8'),{eval:true});const timer=setTimeout(()=>{console.error('JavaScript fixture timed out');process.exitCode=1;worker.terminate();},2000);worker.on('error',error=>{console.error(error);process.exitCode=1;});worker.on('exit',code=>{clearTimeout(timer);if(code)process.exitCode=1;});"])
        .stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped())
        .spawn().expect("Node.js must be installed for execution tests");
    child
        .stdin
        .take()
        .expect("piped stdin")
        .write_all(code.as_bytes())
        .expect("write fixture");
    let output = child.wait_with_output().expect("Node finishes the fixture");
    assert!(
        output.status.success(),
        "JavaScript failed: {}\n{code}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("fixture output is UTF-8")
}

/// Check an independent oracle; its assertions and expected values are never obfuscated.
///
/// # Panics
/// Fails if the source or transformed program violates its stated contract.
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) fn assert_execution(source: &str, assertions: &str) {
    execute(&format!(
        "{source}\n;const assert = require('node:assert/strict');\n{assertions}"
    ));
}

/// Validate behavior and transformation evidence for the same output at every seed.
///
/// # Panics
/// Fails on invalid fixtures, semantic changes or missing transformations.
#[expect(
    clippy::expect_used,
    reason = "each source fixture must obfuscate successfully"
)]
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) fn assert_behavior(
    source: &str,
    assertions: &str,
    mut options: Options,
    inspect: impl Fn(&ObfuscationOutput),
) {
    for seed in SEEDS {
        options.seed = Some(seed);
        let output = obfuscate_with_report(source, &options).expect("valid source fixture");
        assert_execution(&output.code, assertions);
        inspect(&output);
    }
}
