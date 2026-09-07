use std::fmt::Write;

use crate::common::{self, structure::Structure};
use hide_my_js::{obfuscate, Options, Pass, SkipReason, SourceKind};

/// Every advertised combination must preserve behavior and activate selected passes.
///
/// # Panics
/// Fails if passes interfere, selection is ignored or an enabled pass becomes a no-op.
#[test]
fn pass_combinations_preserve_behavior_and_activate_selected_transformations() {
    let source = r"
        function run(input) {
            'use strict';
            var total = 1, label = 'start';
            while (input > 0) {
                if (input % 2) { total += input; label += 'o'; }
                else { total *= 2; label += 'e'; }
                input--;
            }
            return [this === undefined, {total, label}, 0.1, 4294967295, () => label];
        }
    ";
    for mask in 0..(1 << Pass::ALL.len()) {
        let passes: Vec<_> = Pass::ALL
            .into_iter()
            .enumerate()
            .filter_map(|(index, pass)| (mask & (1 << index) != 0).then_some(pass))
            .collect();
        let flow = passes.contains(&Pass::ControlFlow);
        let strings = passes.contains(&Pass::String);
        let rename = passes.contains(&Pass::Renaming);
        let dead = passes.contains(&Pass::DeadCode);
        common::assert_behavior(
            source,
            r"
            const result = run(3);
            assert.deepEqual(result.slice(0,4), [true,{total:9,label:'startoeo'},0.1,4294967295]);
            assert.equal(result[4](), 'startoeo');
            assert.deepEqual(run(0).slice(0,2), [true,{total:1,label:'start'}]);
        ",
            Options {
                passes,
                ..Default::default()
            },
            |output| {
                let ast = Structure::read(&output.code);
                assert_eq!(
                    !ast.switches.is_empty(),
                    flow,
                    "flow selection, mask {mask}"
                );
                assert_eq!(
                    !ast.strings.contains("start"),
                    strings,
                    "string selection, mask {mask}"
                );
                assert_eq!(
                    !ast.bindings.contains("total"),
                    rename,
                    "renaming selection, mask {mask}"
                );
                assert_eq!(output.report.contextual_strings > 0, flow && strings);
                if flow {
                    assert_eq!(
                        ast.structured_branches > 0,
                        dead,
                        "dead-code selection, mask {mask}"
                    );
                }
            },
        );
    }
}

/// Seed isolation and canonical pass selection are public reproducibility guarantees.
///
/// # Panics
/// Fails if another invocation, selection order or duplicates alter seeded output.
#[test]
fn seeded_output_is_repeatable_and_pass_selection_is_canonical() {
    let source = "function run(input) { var local=input; local++; return 'value'+local; }";
    let options = Options {
        seed: Some(123),
        ..Default::default()
    };
    let first = common::transform(source, &options);
    let other = common::transform(
        source,
        &Options {
            seed: Some(456),
            ..options.clone()
        },
    );
    assert_ne!(
        first, other,
        "this obfuscatable fixture should vary across seeds"
    );
    assert_eq!(
        first,
        common::transform(source, &options),
        "another run must not contaminate seed state"
    );
    let reordered = Options {
        passes: Pass::ALL.into_iter().rev().chain(Pass::ALL).collect(),
        ..options
    };
    assert_eq!(first, common::transform(source, &reordered));
}

/// Validation must be exposed even with passes disabled, and respect the selected source mode.
///
/// # Panics
/// Fails if the library suppresses parser/semantic errors or chooses the wrong grammar.
#[test]
fn validation_and_source_modes_are_enforced_without_transformations() {
    let options = Options {
        passes: vec![],
        ..Default::default()
    };
    for source in [
        "const = ;",
        "let duplicate; let duplicate;",
        "const x: number = 1",
        "const x = <div />",
    ] {
        let error =
            obfuscate(source, &options).expect_err("invalid public API input must be rejected");
        assert!(
            !error.diagnostics().is_empty(),
            "diagnostics must remain inspectable"
        );
        assert!(
            !error.to_string().is_empty(),
            "diagnostics must be printable"
        );
    }
    obfuscate("return 1;", &options).expect("CommonJS permits top-level return");
    obfuscate(
        "return 1;",
        &Options {
            source_kind: SourceKind::Script,
            ..options.clone()
        },
    )
    .expect_err("scripts reject top-level return");
    let module = "export const value = await Promise.resolve(1);";
    obfuscate(module, &options).expect_err("CommonJS rejects modules");
    obfuscate(
        module,
        &Options {
            source_kind: SourceKind::Module,
            ..options
        },
    )
    .expect("modules support exports and await");
}

/// Dynamic scope must be guarded independently for eval and with, including string helpers.
///
/// # Panics
/// Fails if unsafe passes change dynamically resolved names or introduce helpers.
#[test]
fn eval_and_with_preserve_dynamic_name_resolution() {
    for (source, assertions) in [
        (
            "function run() { let value=3; return eval('value'); }",
            "assert.equal(run(),3);",
        ),
        (
            "function run() { let value=4; with({ value: 8 }) { return value; } }",
            "assert.equal(run(),8);",
        ),
    ] {
        let unchanged = common::transform(
            source,
            &Options {
                passes: vec![],
                ..Default::default()
            },
        );
        common::assert_behavior(
            source,
            assertions,
            Options {
                passes: vec![
                    Pass::ControlFlow,
                    Pass::DeadCode,
                    Pass::String,
                    Pass::Renaming,
                ],
                ..Default::default()
            },
            |output| {
                assert_eq!(
                    output.code, unchanged,
                    "dynamic scope must disable all helper-producing passes"
                );
                assert!(output
                    .report
                    .skipped
                    .iter()
                    .any(|skip| skip.reason == SkipReason::DynamicScope));
            },
        );
    }
}

/// Suspension and iteration protocols must keep next/return/finally behavior intact.
///
/// # Panics
/// Fails if a suspended frame or iterator close is changed by the pipeline.
#[test]
fn suspension_and_iterator_closing_preserve_control_transfers() {
    common::assert_behavior(
        r"
        var log = [];
        function* values() { try { yield 'first'; yield 'second'; } finally { log.push('closed'); } }
        function collect() { for (const value of values()) { log.push(value); break; } return log; }
        async function read() { const value = await Promise.resolve('ready'); return value + 'done'; }
    ",
        r"
        assert.deepEqual(collect(), ['first','closed']);
        const iterator = values();
        assert.deepEqual(iterator.next(), {value:'first',done:false});
        assert.deepEqual(iterator.next(), {value:'second',done:false});
        assert.deepEqual(iterator.return('stop'), {value:'stop',done:true});
        let settled = false;
        read().then(value => { assert.equal(value,'readydone'); settled = true; }).catch(error => { throw error; });
        process.once('beforeExit', () => assert.equal(settled, true, 'async fixture must complete'));
    ",
        Options::default(),
        |output| {
            assert!(output
                .report
                .skipped
                .iter()
                .any(|skip| skip.reason == SkipReason::Suspension));
        },
    );
}

/// Imports, exported aliases and initialization order are part of the module's public API.
///
/// # Panics
/// Fails if rewriting changes the module namespace or evaluates exports incorrectly.
#[test]
fn modules_preserve_imports_export_names_and_initializers() {
    let source = r"
        import { basename as base } from 'node:path';
        export const publicValue = 'visible';
        export function calculate(input) { let local=input+2; local++; local*=2; return base('/tmp/'+publicValue)+local; }
        export { calculate as alias };
        export default calculate(3);
    ";
    let output = common::transform(
        source,
        &Options {
            source_kind: SourceKind::Module,
            seed: Some(0),
            ..Default::default()
        },
    );
    let encoded = hex(&output);
    let script = format!(
        r"
        import('data:text/javascript;base64,' + Buffer.from('{encoded}','hex').toString('base64'))
            .then(m => console.log(Object.keys(m).join(','), m.default, m.alias(1)))
            .catch(error => {{ throw error; }});
    "
    );
    assert_eq!(
        common::execute(&script).trim(),
        "alias,calculate,default,publicValue visible12 visible8"
    );
}

/// A cyclic import can call a hoisted export before its module's initializers run.
///
/// # Panics
/// Fails if pooled decoding requires a top-level initializer to run first.
#[test]
fn module_cycles_can_call_hoisted_functions_before_initialization() {
    let first = "import {early} from './b.mjs'; export function read(){ var x='cycle'; x+='safe'; return x; } export const result=early;";
    let second = "import {read} from './a.mjs'; export const early=read();";
    for passes in [vec![Pass::String], Pass::ALL.to_vec()] {
        let options = Options {
            passes,
            source_kind: SourceKind::Module,
            seed: Some(0),
            ..Default::default()
        };
        let first = hex(&common::transform(first, &options));
        let second = hex(&common::transform(second, &options));
        let script = format!(
            r"
            const fs=require('node:fs'), os=require('node:os'), path=require('node:path');
            const dir=fs.mkdtempSync(path.join(os.tmpdir(),'hide-my-js-cycle-'));
            fs.writeFileSync(path.join(dir,'a.mjs'),Buffer.from('{first}','hex'));
            fs.writeFileSync(path.join(dir,'b.mjs'),Buffer.from('{second}','hex'));
            import(require('node:url').pathToFileURL(path.join(dir,'a.mjs')).href)
                .then(m=>console.log(m.result)).catch(error=>{{throw error;}})
                .finally(()=>fs.rmSync(dir,{{recursive:true,force:true}}));
        "
        );
        assert_eq!(common::execute(&script).trim(), "cyclesafe");
    }
}

/// Transport module source as UTF-8 bytes without introducing a second escaping interpretation.
#[expect(clippy::expect_used, reason = "formatting into String cannot fail")]
fn hex(source: &str) -> String {
    let mut result = String::new();
    for byte in source.bytes() {
        write!(result, "{byte:02x}").expect("infallible String formatting");
    }
    result
}
