//! End-to-end contracts for CLI option mapping, streams, and file handling.
#![cfg(feature = "cli")]

use std::{
    ffi::OsStr,
    fs,
    io::Write,
    path::PathBuf,
    process::{Command, Output, Stdio},
};

use hide_my_js::{obfuscate, Options, Pass, SourceKind};
use {clap as _, oxc as _};

/// Execute the actual binary with optional piped source.
///
/// # Panics
/// Fails if the test binary or its standard streams are unavailable.
#[expect(
    clippy::expect_used,
    reason = "the fixture binary and its piped I/O must be available"
)]
fn invoke(args: impl IntoIterator<Item = impl AsRef<OsStr>>, source: Option<&str>) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_hide-my-js"))
        .args(args)
        .stdin(if source.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("CLI test binary must start");
    if let Some(source) = source {
        child
            .stdin
            .take()
            .expect("source requires piped stdin")
            .write_all(source.as_bytes())
            .expect("CLI must accept the small input fixture");
    }
    child
        .wait_with_output()
        .expect("CLI test binary must complete")
}

/// CLI parsing must forward defaults, pass choices, limits and seeds correctly.
///
/// # Panics
/// Fails if stdout differs from the library result or the report pollutes it.
#[test]
fn streams_and_options_match_the_library() {
    let source =
        "export function run(n) { var text='ready'; text+='a'; text+='b'; text+='c'; while(n--) { text+='!'; } return [text, 42]; }";
    let base = Options {
        source_kind: SourceKind::Module,
        seed: Some(7),
        ..Default::default()
    };
    let default = obfuscate(source, &base).expect("valid module fixture");
    for (extra, options) in option_cases(&base) {
        let mut args = vec![
            "-",
            "-o",
            "-",
            "--source-kind",
            "module",
            "--seed",
            "7",
            "--report",
        ];
        let expected = obfuscate(source, &options).expect("valid module fixture");
        if !extra.is_empty() {
            assert_ne!(
                expected, default,
                "fixture must expose ignored flags: {extra:?}"
            );
        }
        args.extend(extra);
        let output = invoke(args, Some(source));
        assert!(
            output.status.success(),
            "CLI failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(output.stdout, expected.as_bytes());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("dispatchers:"),
            "report belongs on stderr"
        );
    }
}

/// Exercise each CLI mapping separately so one option cannot mask another being ignored.
fn option_cases(base: &Options) -> Vec<(Vec<&'static str>, Options)> {
    let mut cases = vec![(vec![], base.clone())];
    for (name, pass) in [
        ("control-flow", Pass::ControlFlow),
        ("dead-code", Pass::DeadCode),
        ("numeric", Pass::Numeric),
        ("string", Pass::String),
        ("renaming", Pass::Renaming),
    ] {
        cases.push((
            vec!["--passes", name],
            Options {
                passes: vec![pass],
                ..base.clone()
            },
        ));
    }
    cases.extend([
        (
            vec!["--passes", "string,control-flow", "--passes", "renaming"],
            Options {
                passes: vec![Pass::String, Pass::ControlFlow, Pass::Renaming],
                ..base.clone()
            },
        ),
        (
            vec!["--control-flow-coverage", "0"],
            Options {
                control_flow_coverage: 0,
                ..base.clone()
            },
        ),
        (
            vec!["--max-dispatcher-blocks", "3"],
            Options {
                max_dispatcher_blocks: 3,
                ..base.clone()
            },
        ),
        (
            vec!["--max-string-pool-entries", "1"],
            Options {
                max_string_pool_entries: 1,
                ..base.clone()
            },
        ),
        (
            vec!["--no-passes"],
            Options {
                passes: vec![],
                ..*base
            },
        ),
    ]);
    cases
}

/// Omitted paths use standard streams and the default parsing mode is `CommonJS`.
///
/// # Panics
/// Fails if stream defaults or the selected parsing mode differ from the library.
#[test]
fn default_streams_and_source_modes_match_the_library() {
    let script = "return 'common-js';";
    let output = invoke(["--seed", "7"], Some(script));
    assert!(
        output.status.success(),
        "default mode must permit CommonJS return"
    );
    assert!(output.stderr.is_empty(), "normal success must be quiet");
    assert_eq!(
        output.stdout,
        obfuscate(
            script,
            &Options {
                seed: Some(7),
                ..Default::default()
            }
        )
        .expect("valid CommonJS fixture")
        .as_bytes()
    );
    let output = invoke(["--source-kind", "script"], Some(script));
    assert_eq!(
        output.status.code(),
        Some(1),
        "script mode rejects top-level return"
    );
    assert!(output.stdout.is_empty());
}

/// Temporary files used to test the CLI's own read/write behavior.
struct Directory {
    /// Exclusively created directory, removed when the fixture leaves scope.
    path: PathBuf,
}

impl Directory {
    /// Create a unique directory without adding a test-only dependency.
    ///
    /// # Panics
    /// Fails if the operating system cannot create fixture storage.
    #[expect(
        clippy::expect_used,
        reason = "the test requires writable temporary storage"
    )]
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "hide-my-js-cli-{}-{}",
            std::process::id(),
            rand::random::<u64>()
        ));
        fs::create_dir(&path).expect("unique CLI fixture directory must be created");
        Self { path }
    }
}

impl Drop for Directory {
    #[expect(
        clippy::let_underscore_must_use,
        reason = "best-effort fixture cleanup must not mask a test failure"
    )]
    fn drop(&mut self) {
        let _: std::io::Result<()> = fs::remove_dir_all(&self.path);
    }
}

/// File output occurs only after the complete source has been read and validated.
///
/// # Panics
/// Fails on incorrect file output, lost input, or truncation after validation failure.
#[test]
fn files_support_same_path_and_preserve_output_on_invalid_input() {
    let directory = Directory::new();
    let input = directory.path.join("input.js");
    let destination = directory.path.join("output.js");
    let source = "console.log('hello');";
    fs::write(&input, source).expect("write source fixture");
    let output = invoke(
        [
            input.as_os_str(),
            OsStr::new("-o"),
            destination.as_os_str(),
            OsStr::new("--seed"),
            OsStr::new("42"),
        ],
        None,
    );
    assert!(output.status.success(), "file transformation must succeed");
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
    let expected = obfuscate(
        source,
        &Options {
            seed: Some(42),
            ..Default::default()
        },
    )
    .expect("valid file fixture");
    assert_eq!(
        fs::read_to_string(&destination).expect("read generated file"),
        expected
    );

    let output = invoke(
        [
            input.as_os_str(),
            OsStr::new("-o"),
            input.as_os_str(),
            OsStr::new("--seed"),
            OsStr::new("42"),
        ],
        None,
    );
    assert!(
        output.status.success(),
        "same-path output must read the original first"
    );
    assert_eq!(
        fs::read_to_string(&input).expect("read same-path output"),
        expected
    );

    fs::write(&input, "const = ;").expect("write invalid source fixture");
    let output = invoke(
        [input.as_os_str(), OsStr::new("-o"), destination.as_os_str()],
        None,
    );
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("input.js"));
    assert_eq!(
        fs::read_to_string(&destination).expect("existing destination must survive"),
        expected
    );
}

/// File I/O errors must fail without emitting JavaScript.
///
/// # Panics
/// Fails if errors are silently accepted or written to stdout.
#[test]
fn file_errors_have_nonzero_status_and_stderr_diagnostics() {
    let directory = Directory::new();
    let missing = directory.path.join("missing.js");
    let output = invoke([missing.as_os_str()], None);
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("cannot read"));

    let output = invoke([OsStr::new("-o"), directory.path.as_os_str()], Some("1;"));
    assert_eq!(output.status.code(), Some(1));
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("cannot write"));
}
