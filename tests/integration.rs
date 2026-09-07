//! JavaScript runtime, pipeline, and public API regressions.

#[cfg(feature = "cli")]
use clap as _;
use rand as _;

/// JavaScript fixtures covering the pipeline and individual passes.
mod cases;
/// Shared validation and Node execution helpers.
mod common;
