use crate::common::{self, structure::Structure};
use hide_my_js::{Options, Pass};

/// Int32 masking must be active, while values outside its domain stay exact.
///
/// # Panics
/// Fails on signed-zero loss, truncation, precedence changes or a disabled numeric pass.
#[test]
fn masks_integers_without_coercing_other_javascript_numbers() {
    common::assert_behavior(
        r"
        const values = [0, -0, 1, -1, 2147483647, -2147483648, 2147483648,
            4294967295, 9007199254740991, 0.1, 1.0000000000000002, 5e-324, 1e400];
        const powers = [2 ** 3, (-2) ** 3, 42 .toString(), 12345678901234567890n];
        const keys = {42: 'answer'};
    ",
        r"
        assert.deepEqual(values, [0, -0, 1, -1, 2147483647, -2147483648, 2147483648,
            4294967295, 9007199254740991, 0.1, 1.0000000000000002, Number.MIN_VALUE, Infinity]);
        assert.deepEqual(powers, [8, -8, '42', 12345678901234567890n]);
        assert.equal(keys[42], 'answer');
    ",
        Options {
            passes: vec![Pass::Numeric],
            ..Default::default()
        },
        |output| {
            assert!(
                Structure::read(&output.code).xors > 0,
                "eligible integers must actually be masked"
            );
        },
    );
}
