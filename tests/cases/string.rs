use std::collections::HashSet;

use crate::common::{self, structure::Structure};
use hide_my_js::{Options, Pass, SkipReason};
use oxc::{
    allocator::Allocator,
    ast::ast::{CallExpression, Expression, IdentifierReference, SwitchStatement},
    ast_visit::{walk, Visit},
    parser::Parser,
    span::SourceType,
};

/// Plaintext must disappear without altering UTF-16 values or syntax-sensitive positions.
///
/// # Panics
/// Fails on changed string values, directives, keys or tagged-template raw contents.
#[test]
fn encodes_utf16_values_and_preserves_directives_keys_and_tagged_templates() {
    common::assert_behavior(
        r#"
        function values() {
            return ["pool-secret", "é汉字😀", "\\n", "\n", "\0", "` ${x}", "\u2028\u2029", "\ud800", "\udfff", "�", ""];
        }
        function strict() { 'use strict'; return this === undefined; }
        const object = {"stable-key": "value", "__proto__": null};
        const raw = String.raw`\x41`;
    "#,
        r#"
        assert.deepEqual(values(), ["pool-secret", "é汉字😀", "\\n", "\n", "\0", "` ${x}", "\u2028\u2029", "\ud800", "\udfff", "�", ""]);
        assert.equal(strict(), true);
        assert.equal(object['stable-key'], 'value');
        assert.equal(Object.getPrototypeOf(object), null);
        assert.equal(raw, '\\x41');
    "#,
        Options {
            passes: vec![Pass::String],
            ..Default::default()
        },
        |output| {
            let ast = Structure::read(&output.code);
            assert!(
                !ast.strings.contains("pool-secret"),
                "escaping a plaintext template is not encoded pooling"
            );
            assert!(
                ast.strings.contains("stable-key"),
                "literal property keys retain their syntax"
            );
            assert!(output.report.encoded_strings > 0);
        },
    );
}

/// Hoisted calls and parameter evaluation cannot depend on body-helper initialization.
///
/// # Panics
/// Fails on initialization order, shadowed built-ins or actual pool-size violations.
#[test]
fn pools_obey_entry_limits_and_work_before_body_initialization() {
    let source = r#"
        var first = early();
        function early(value = "parameter", String = null, Array = null, Object = null) {
            return [value, "pool-secret", "\ud800", "�\ud800�", "😀", "", "\\n"];
        }
    "#;
    for limit in [0, 2, 32] {
        common::assert_behavior(
            source,
            r#"
            assert.deepEqual(first, ["parameter", "pool-secret", "\ud800", "�\ud800�", "😀", "", "\\n"]);
            assert.deepEqual(early('supplied').slice(1), first.slice(1));
        "#,
            Options {
                passes: vec![Pass::String],
                max_string_pool_entries: limit,
                ..Default::default()
            },
            |output| {
                let ast = Structure::read(&output.code);
                if limit == 0 {
                    assert_eq!(output.report.encoded_strings, 0);
                    assert!(
                        ast.objects.is_empty(),
                        "disabled pooling must not inject tables"
                    );
                    assert!(ast.strings.contains("pool-secret"));
                } else {
                    assert!(!ast.strings.contains("pool-secret"));
                    assert!(
                        ast.strings.contains("parameter"),
                        "parameters cannot call body helpers"
                    );
                    assert_eq!(
                        ast.objects.iter().sum::<usize>(),
                        6,
                        "all six eligible occurrences belong to pools"
                    );
                    assert!(
                        ast.objects.iter().all(|size| *size <= limit),
                        "every emitted table must respect the cap"
                    );
                    assert_eq!(output.report.string_pools, ast.objects.len());
                    assert!(output
                        .report
                        .skipped
                        .iter()
                        .any(|skip| skip.reason == SkipReason::Parameters));
                }
            },
        );
    }
}

/// Coupled decoding must use real state and still survive joins and independent invocations.
///
/// # Panics
/// Fails on missing state inputs, wrong keys, shared call state or deferred closure decoding.
#[test]
fn contextual_decoding_survives_joins_recursion_reentrancy_and_deferred_calls() {
    let source = r"
        function run(n, callback) {
            var text = 'start', i = 0, deferred;
            if (n > 0) text += run(n - 1, () => 'recursive').text;
            else text += 'base';
            while (i++ < 3) { if (i === 2) text += 'middle'; else text += 'side'; }
            deferred = () => 'deferred';
            text += callback();
            return {text: text + 'end', deferred};
        }
    ";
    for passes in [
        vec![Pass::String],
        vec![Pass::ControlFlow, Pass::String],
        Pass::ALL.to_vec(),
    ] {
        let coupled = passes.contains(&Pass::ControlFlow);
        common::assert_behavior(
            source,
            r"
            const result = run(1, () => run(0, () => 'reentrant').text);
            assert.equal(result.text, 'start' + 'startbasesidemiddlesiderecursiveend' + 'sidemiddleside' + 'startbasesidemiddlesidereentrantend' + 'end');
            assert.equal(result.deferred(), 'deferred');
            assert.equal(run(0, () => 'next').text, 'startbasesidemiddlesidenextend');
        ",
            Options {
                passes,
                ..Default::default()
            },
            |output| {
                assert!(!Structure::read(&output.code).strings.contains("start"));
                let calls = coupled_calls(&output.code);
                if coupled {
                    assert!(
                        calls > 0,
                        "combined decoder calls must read dispatcher bindings"
                    );
                } else {
                    assert_eq!(calls, 0);
                }
                assert_eq!(
                    calls, output.report.contextual_strings,
                    "report and emitted call arguments must agree"
                );
            },
        );
    }
}

/// Count emitted decoder keys that reference both members of a dispatcher's state pair.
///
/// # Panics
/// Fails if generated JavaScript is invalid.
fn coupled_calls(source: &str) -> usize {
    let allocator = Allocator::default();
    let parsed = Parser::new(&allocator, source, SourceType::cjs()).parse();
    assert!(
        parsed.diagnostics.is_empty(),
        "generated decoder syntax must be valid"
    );
    let mut states = States::default();
    states.visit_program(&parsed.program);
    let mut calls = Calls { states, coupled: 0 };
    calls.visit_program(&parsed.program);
    calls.coupled
}
/// Collect state pairs from generated switch discriminants.
#[derive(Default)]
struct States {
    /// Names whose XOR selects each switch case.
    pairs: Vec<(String, String)>,
}
impl<'a> Visit<'a> for States {
    fn visit_switch_statement(&mut self, switch: &SwitchStatement<'a>) {
        if let Expression::BinaryExpression(binary) = switch.discriminant.get_inner_expression() {
            if let (Expression::Identifier(left), Expression::Identifier(right)) = (
                binary.left.get_inner_expression(),
                binary.right.get_inner_expression(),
            ) {
                self.pairs
                    .push((left.name.to_string(), right.name.to_string()));
            }
        }
        walk::walk_switch_statement(self, switch);
    }
}
/// Collect references in one argument without examining printed text patterns.
#[derive(Default)]
struct References {
    /// Referenced identifier spellings after optional renaming.
    names: HashSet<String>,
}
impl<'a> Visit<'a> for References {
    fn visit_identifier_reference(&mut self, reference: &IdentifierReference<'a>) {
        self.names.insert(reference.name.to_string());
    }
}
/// Count actual decoder calls using both state variables as key inputs.
struct Calls {
    /// Dispatcher state bindings.
    states: States,
    /// Number of calls consuming a complete pair.
    coupled: usize,
}
impl<'a> Visit<'a> for Calls {
    fn visit_call_expression(&mut self, call: &CallExpression<'a>) {
        if let Some(argument) = call
            .arguments
            .get(1)
            .and_then(oxc::ast::ast::Argument::as_expression)
        {
            let mut references = References::default();
            references.visit_expression(argument);
            if self.states.pairs.iter().any(|(left, right)| {
                references.names.contains(left) && references.names.contains(right)
            }) {
                self.coupled += 1;
            }
        }
        walk::walk_call_expression(self, call);
    }
}
