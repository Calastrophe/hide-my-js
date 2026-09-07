use crate::common::{self, structure::Structure};
use hide_my_js::{Options, Pass, SkipReason};

/// Lowering must preserve condition side effects, loop targets and abrupt completion.
///
/// # Panics
/// Fails on wrong execution or if eligible transfers remain structured.
#[test]
fn lowers_branches_loops_and_labels_without_changing_evaluation_order() {
    let source = r"
        function run(limit) {
            var log = [], reads = 0, i = 0, j = 0;
            var probe = { get next() { reads++; return i < limit; } };
            outer: again: while (probe.next) {
                i++;
                for (j = 0; j < 3; j++) {
                    if (j === 1 && i === 2) continue outer;
                    if (i === 3) break again;
                    log.push(i * 10 + j);
                }
            }
            do { i--; if (i === 1) continue; log.push(i); } while (i > 0);
            stop: { if (limit > 0) break stop; log.push(99); }
            if (limit < 0) throw 'negative';
            if (limit === 0) return [log, reads, 'early'];
            return [log, reads, 'done'];
        }
    ";
    common::assert_behavior(
        source,
        r"
        assert.deepEqual(run(4), [[10,11,12,20,2,0],3,'done']);
        assert.deepEqual(run(0), [[-1,99],1,'early']);
        assert.throws(() => run(-1), error => error === 'negative');
    ",
        Options {
            passes: vec![Pass::ControlFlow],
            ..Default::default()
        },
        |output| {
            let ast = Structure::read(&output.code);
            assert!(
                !ast.switches.is_empty(),
                "eligible code must become dispatchers"
            );
            assert_eq!(
                ast.structured_branches, 0,
                "eligible branches belong inside dispatchers"
            );
            assert_eq!(
                (ast.labels, ast.continues, ast.do_loops),
                (0, 0, 0),
                "local transfers and do/while must be lowered"
            );
            assert_eq!(ast.switches.len(), output.report.flattened_regions);
        },
    );
}

/// Moving declarations or transfers across boundaries changes TDZ, captures and finally.
///
/// # Panics
/// Fails when a scope or exception boundary loses its original behavior.
#[test]
fn retains_lexical_initialization_iteration_captures_and_finally() {
    common::assert_behavior(
        r"
        function run() {
            let log = [], closures = [];
            for (let i = 0; i < 3; i++) {
                const value = (log.push('init' + i), i);
                closures.push(() => value);
                if (i === 1) continue;
                log.push(value);
            }
            try { log.push(x); let x = 1; } catch (error) { log.push(error.name); }
            outer: for (let i = 0; i < 2; i++) {
                try { log.push('try'); continue outer; }
                finally { log.push('finally'); }
            }
            try { return [log, closures.map(f => f())]; }
            finally { log.push('return-finally'); }
        }
    ",
        r"
        assert.deepEqual(run(), [
            ['init0',0,'init1','init2',2,'ReferenceError','try','finally','try','finally','return-finally'],
            [0,1,2]
        ]);
    ",
        Options::default(),
        |output| {
            assert!(output
                .report
                .skipped
                .iter()
                .any(|skip| skip.reason == SkipReason::LexicalScope));
            assert!(output
                .report
                .skipped
                .iter()
                .any(|skip| skip.reason == SkipReason::ExternalTransfer));
        },
    );
}

/// Configuration must bound actual emitted switches, including their synthetic exits.
///
/// # Panics
/// Fails if coverage is ignored or any dispatcher exceeds its configured bound.
#[test]
fn coverage_and_block_limits_control_emitted_dispatchers() {
    let source = "function run() { var x=0; x++; x+=2; x*=3; x-=1; x++; x+=2; return x; }";
    for limit in [2, 3, 5] {
        common::assert_behavior(
            source,
            "assert.equal(run(), 11);",
            Options {
                passes: vec![Pass::ControlFlow],
                max_dispatcher_blocks: limit,
                ..Default::default()
            },
            |output| {
                let ast = Structure::read(&output.code);
                if limit < 3 {
                    assert!(
                        ast.switches.is_empty(),
                        "too-small limits must disable flattening"
                    );
                } else {
                    assert!(
                        !ast.switches.is_empty(),
                        "the limit must still allow small regions"
                    );
                    assert!(
                        ast.switches.iter().all(|cases| *cases < limit),
                        "the exit also counts against the block cap"
                    );
                }
                assert_eq!(output.report.flattened_regions, ast.switches.len());
            },
        );
    }
    common::assert_behavior(
        source,
        "assert.equal(run(), 11);",
        Options {
            passes: vec![Pass::ControlFlow],
            control_flow_coverage: 0,
            ..Default::default()
        },
        |output| {
            assert!(Structure::read(&output.code).switches.is_empty());
            assert!(output
                .report
                .skipped
                .iter()
                .any(|skip| skip.reason == SkipReason::Coverage));
        },
    );
}
