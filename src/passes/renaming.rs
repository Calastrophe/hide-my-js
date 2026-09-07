use std::collections::HashMap;

use oxc::{
    allocator::{ArenaBox, TakeIn},
    ast::{
        ast::{
            AssignmentTarget, AssignmentTargetMaybeDefault, AssignmentTargetProperty,
            BindingIdentifier, BindingProperty, IdentifierReference, ObjectProperty, Program,
            PropertyKey,
        },
        builder::AstBuilder,
    },
    ast_visit::{walk_mut, VisitMut},
    semantic::{Scoping, SymbolId},
    str::Ident,
};

use super::Context;

/// Assign replacement names by symbol, using the pipeline's current semantic IDs.
pub(super) fn rename<'a>(program: &mut Program<'a>, scoping: &Scoping, context: &mut Context<'a>) {
    let mut symbols = HashMap::new();
    for symbol in scoping.symbol_ids() {
        // Preserve the external surface, special arguments behavior, and
        // function declarations with sloppy-mode block hoisting semantics.
        if scoping.symbol_scope_id(symbol) == scoping.root_scope_id()
            || scoping.symbol_name(symbol) == "arguments"
            || scoping.symbol_flags(symbol).is_function()
        {
            continue;
        }
        symbols.insert(symbol, context.fresh_name());
    }
    Renamer {
        ast: &context.ast,
        scoping,
        symbols,
    }
    .visit_program(program);
}

/// Rewrite bindings and references while preserving public property names.
struct Renamer<'ctx, 'a> {
    /// Arena used when expanding shorthand assignments.
    ast: &'ctx AstBuilder<'a>,
    /// Owned scope data supplied by the pipeline, independent of the mutable AST.
    scoping: &'ctx Scoping,
    /// One replacement name for each renamed symbol.
    symbols: HashMap<SymbolId, Ident<'a>>,
}

impl<'a> VisitMut<'a> for Renamer<'_, 'a> {
    fn visit_binding_identifier(&mut self, identifier: &mut BindingIdentifier<'a>) {
        if let Some(name) = identifier
            .symbol_id
            .get()
            .and_then(|id| self.symbols.get(&id))
        {
            identifier.name = *name;
        }
    }

    fn visit_identifier_reference(&mut self, identifier: &mut IdentifierReference<'a>) {
        if let Some(name) = identifier
            .reference_id
            .get()
            .and_then(|id| self.scoping.get_reference(id).symbol_id())
            .and_then(|id| self.symbols.get(&id))
        {
            identifier.name = *name;
        }
    }

    fn visit_object_property(&mut self, property: &mut ObjectProperty<'a>) {
        if property.shorthand
            && matches!(&property.key, PropertyKey::StaticIdentifier(key) if key.name == "__proto__")
        {
            property.key =
                PropertyKey::new_string_literal(property.span, "__proto__", None, self.ast);
            property.computed = true;
        }
        property.shorthand = false;
        walk_mut::walk_object_property(self, property);
    }

    fn visit_binding_property(&mut self, property: &mut BindingProperty<'a>) {
        property.shorthand = false;
        walk_mut::walk_binding_property(self, property);
    }

    fn visit_assignment_target_property(&mut self, property: &mut AssignmentTargetProperty<'a>) {
        if let AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(short) = property {
            let ast = self.ast;
            let span = short.span;
            let key = PropertyKey::new_static_identifier(span, short.binding.name, ast);
            let target = AssignmentTarget::AssignmentTargetIdentifier(ArenaBox::new_in(
                short.binding.take_in(ast),
                ast,
            ));
            let binding = if let Some(init) = short.init.take() {
                AssignmentTargetMaybeDefault::new_assignment_target_with_default(
                    span, target, init, ast,
                )
            } else {
                target.into()
            };
            *property = AssignmentTargetProperty::new_assignment_target_property_property(
                span, key, binding, false, ast,
            );
        }
        walk_mut::walk_assignment_target_property(self, property);
    }
}
