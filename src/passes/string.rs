use std::{collections::HashSet, fmt::Write};

use oxc::{
    allocator::ArenaVec,
    ast::ast::{
        Expression, FormalParameters, FunctionBody, Program, PropertyKey, Statement, StringLiteral,
    },
    ast_visit::{walk_mut, VisitMut},
    str::Ident,
};
use rand::{seq::SliceRandom, Rng};

use super::Context;
use crate::SkipReason;

/// One occurrence, encoded independently even when its plaintext is duplicated.
struct Entry {
    /// Random logical index in this pool.
    index: i32,
    /// Per-code-unit additive key progression, modulo 65536.
    step: u16,
    /// Encoded UTF-16 code units, including unpaired surrogates.
    units: Vec<u16>,
}

/// A hoisted accessor that lazily captures immutable encoded storage.
struct Pool<'a> {
    /// Collision-free accessor name in the containing scope.
    name: Ident<'a>,
    /// Mask for caller-side indexes.
    mask: i32,
    /// Independently encoded occurrences, shuffled before helper emission.
    entries: Vec<Entry>,
    /// Reserve logical indexes to avoid accidental entry aliasing.
    indexes: HashSet<i32>,
}

/// Plan pooled strings in their original lexical scopes, then append helpers.
pub(super) struct Strings<'ctx, 'a> {
    /// Shared randomness, arena and original block metadata.
    context: &'ctx mut Context<'a>,
    /// Pools belonging to the currently visited program or function body.
    pools: Vec<Pool<'a>>,
    /// Maximum occurrences in a helper.
    limit: usize,
}

impl<'ctx, 'a> Strings<'ctx, 'a> {
    /// Start an independent or context-coupled pooling pass.
    pub(super) const fn new(context: &'ctx mut Context<'a>, limit: usize) -> Self {
        Self {
            context,
            pools: vec![],
            limit,
        }
    }

    /// Encode with a salted block invariant, or a standalone occurrence key.
    fn encode(&mut self, literal: &StringLiteral<'a>) -> Expression<'a> {
        if self
            .pools
            .last()
            .is_none_or(|pool| pool.entries.len() >= self.limit)
        {
            self.pools.push(Pool {
                name: self.context.fresh_name(),
                mask: self.context.rng.random_range(1..=i32::MAX),
                entries: vec![],
                indexes: HashSet::new(),
            });
        }
        let salt = self.context.rng.random::<u16>();
        let (key, key_expression) =
            if let Some(state) = self.context.string_contexts.get(&literal.span).copied() {
                self.context.report.contextual_strings += 1;
                let key = low_u16(state.identity) ^ salt;
                (key, format!("({} ^ {salt}) & 65535", state.invariant()))
            } else {
                let key = self.context.rng.random::<u16>();
                (key, key.to_string())
            };
        let step = self.context.rng.random::<u16>() | 1;
        let mut current = key;
        let units = utf16(literal)
            .into_iter()
            .map(|unit| {
                let encoded = unit ^ current;
                current = current.wrapping_add(step);
                encoded
            })
            .collect();
        let Some(pool) = self.pools.last_mut() else {
            return Expression::new_null_literal(literal.span, &self.context.ast);
        };
        let mut index = self.context.rng.random_range(1..=i32::MAX);
        while !pool.indexes.insert(index) {
            index = self.context.rng.random_range(1..=i32::MAX);
        }
        pool.entries.push(Entry { index, step, units });
        self.context.report.encoded_strings += 1;
        let wrapper_salt = if self.context.rng.random::<bool>() {
            self.context.rng.random::<u16>()
        } else {
            0
        };
        let call = if wrapper_salt == 0 {
            format!("{}({}, {key_expression})", pool.name, index ^ pool.mask)
        } else {
            format!(
                "{}({}, ({key_expression}) ^ {wrapper_salt}, {wrapper_salt})",
                pool.name,
                index ^ pool.mask ^ i32::from(wrapper_salt)
            )
        };
        self.context.expression(&call)
    }

    /// Emit after visiting user AST, keeping generated helpers out of every pass.
    #[expect(
        clippy::let_underscore_must_use,
        reason = "formatting into String is infallible"
    )]
    fn finish_scope(&mut self, statements: &mut ArenaVec<'a, Statement<'a>>) {
        for mut pool in std::mem::take(&mut self.pools) {
            pool.entries.shuffle(&mut self.context.rng);
            let mut entries = String::new();
            for entry in pool.entries {
                let _ = write!(entries, "{}:[{},[", entry.index, entry.step);
                for unit in entry.units {
                    let _ = write!(entries, "{unit},");
                }
                entries.push_str("]],");
            }
            // A hoisted declaration supports early calls and module cycles. Its
            // first call replaces only this generated binding with a closure over
            // immutable ciphertext. Every call still decodes fresh plaintext and
            // keeps its key and cursor local, including recursive/reentrant calls.
            let helper = format!(
                "function {name}(i,k,s=0){{const p={{{entries}}};{name}=function(i,k,s=0){{let e=p[(i^s)^{mask}],r='',j=0;k^=s;for(;j<e[1].length;j++){{r+=''.constructor.fromCharCode(e[1][j]^(k&65535));k=(k+e[0])&65535;}}return r;}};return {name}(i,k,s);}}",
                name = pool.name, mask = pool.mask,
            );
            statements.extend(self.context.statements(&helper));
            self.context.report.string_pools += 1;
        }
    }
}

/// Preserve exactly the low 16 bits of a generated signed state value.
const fn low_u16(value: i32) -> u16 {
    let [first, second, _, _] = value.to_le_bytes();
    u16::from_le_bytes([first, second])
}

/// Decode Oxc's U+FFFD plus four-hex-digit representation of lone surrogates.
fn utf16(literal: &StringLiteral<'_>) -> Vec<u16> {
    if !literal.lone_surrogates {
        return literal.value.encode_utf16().collect();
    }
    let mut units = Vec::new();
    let mut chars = literal.value.chars();
    while let Some(character) = chars.next() {
        if character == '\u{fffd}' {
            let hex: String = chars.by_ref().take(4).collect();
            if let Ok(unit) = u16::from_str_radix(&hex, 16) {
                units.push(unit);
            }
        } else {
            units.extend_from_slice(character.encode_utf16(&mut [0; 2]));
        }
    }
    units
}

impl<'a> VisitMut<'a> for Strings<'_, 'a> {
    fn visit_program(&mut self, program: &mut Program<'a>) {
        walk_mut::walk_program(self, program);
        self.finish_scope(&mut program.body);
    }

    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        let outer = std::mem::take(&mut self.pools);
        walk_mut::walk_function_body(self, body);
        self.finish_scope(&mut body.statements);
        self.pools = outer;
    }

    fn visit_formal_parameters(&mut self, parameters: &mut FormalParameters<'a>) {
        if !parameters.items.is_empty() {
            self.context.skip(parameters.span, SkipReason::Parameters);
        }
    }

    fn visit_property_key(&mut self, key: &mut PropertyKey<'a>) {
        // Preserve __proto__, directives, static imports and all literal keys.
        if !matches!(key, PropertyKey::StringLiteral(_)) {
            walk_mut::walk_property_key(self, key);
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression<'a>) {
        if let Expression::StringLiteral(literal) = expression {
            *expression = self.encode(literal);
        } else {
            walk_mut::walk_expression(self, expression);
        }
    }
}
