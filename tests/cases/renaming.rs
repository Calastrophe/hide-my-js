use crate::common::{self, structure::Structure};
use hide_my_js::{Options, Pass};

/// Symbol resolution must distinguish shadowed bindings and preserve property spellings.
///
/// # Panics
/// Fails on broken captures, destructuring, shorthand properties or missing renaming.
#[test]
fn renames_bindings_without_changing_captures_hoisting_or_property_keys() {
    common::assert_behavior(
        r"
        const external = 11;
        function run(input) {
            var beforeValue = before;
            var before = 7;
            const result = [beforeValue, input];
            { let input = 9; result.push(input); }
            const {key, missing = 5, ...rest} = {key: 4, other: 6};
            let assigned, fallback;
            ({assigned, fallback = input} = {assigned: 8});
            const closure = () => input;
            try { throw 12; } catch (input) { result.push(input); }
            let __proto__ = 7;
            result.push(closure(), {key, missing, rest, assigned, fallback, __proto__}, external);
            return result;
        }
    ",
        r"
        assert.deepEqual(run(2), [undefined,2,9,12,2,
            {key:4,missing:5,rest:{ other: 6 },assigned:8,fallback:2,['__proto__']:7},11]);
    ",
        Options {
            passes: vec![Pass::Renaming],
            ..Default::default()
        },
        |output| {
            let ast = Structure::read(&output.code);
            for local in ["input", "before", "key", "assigned", "closure", "__proto__"] {
                assert!(
                    !ast.bindings.contains(local),
                    "local binding {local} must be renamed"
                );
            }
            assert!(
                ast.bindings.contains("run") && ast.bindings.contains("external"),
                "external names stay stable"
            );
        },
    );
}

/// Sloppy block-function hoisting and mapped arguments are binding-specific contracts.
///
/// # Panics
/// Fails if renaming changes Annex B hoisting, recursive references or arguments aliasing.
#[test]
fn preserves_sloppy_function_hoisting_recursion_and_mapped_arguments() {
    common::assert_behavior(
        r"
        function run(value) {
            if (value) { function local() { return 5; } }
            function factorial(n) { return n ? n * factorial(n - 1) : 1; }
            arguments[0] = 3;
            return [local(), value, factorial(4)];
        }
    ",
        "assert.deepEqual(run(1), [5,3,24]);",
        Options {
            passes: vec![Pass::Renaming],
            ..Default::default()
        },
        |_| {},
    );
}
