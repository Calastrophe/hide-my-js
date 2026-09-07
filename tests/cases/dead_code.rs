use crate::common::{self, structure::Structure};
use hide_my_js::{Options, Pass};

/// Injected branches must exist without changing directives, returns or lexical this.
///
/// # Panics
/// Fails if injection is disabled or alters any live function behavior.
#[test]
fn injects_unreachable_branches_without_changing_function_behavior() {
    common::assert_behavior(
        r"
        function strict() { 'use strict'; return this === undefined; }
        function empty() {}
        const expression = value => value * 2;
        const block = value => { return value + 1; };
        class Box { constructor() { this.value = 6; } read() { return (() => this.value)(); } }
    ",
        r"
        assert.equal(strict(), true);
        assert.equal(empty(), undefined);
        assert.equal(expression(3), 6);
        assert.equal(block(5), 6);
        assert.equal(new Box().read(), 6);
    ",
        Options {
            passes: vec![Pass::DeadCode],
            ..Default::default()
        },
        |output| {
            assert!(
                Structure::read(&output.code).structured_branches > 0,
                "the branch-free input must gain dead code"
            );
        },
    );
}
