use oxc::ast::ast::{BindingIdentifier, IdentifierReference};
use oxc::ast::visit::walk_mut::{walk_binding_identifier, walk_identifier_reference};
use oxc::ast::{AstBuilder, VisitMut};
use std::collections::HashMap;

use crate::utils::identifier::GENERATOR;

pub struct Renamer<'a> {
    ast_builder: &'a AstBuilder<'a>,
    symbol_map: HashMap<&'a str, String>,
}

impl<'a> Renamer<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            symbol_map: HashMap::new(),
        }
    }
}

impl<'a> VisitMut<'a> for Renamer<'a> {
    fn visit_binding_identifier(&mut self, identifier: &mut BindingIdentifier<'a>) {
        let random_name = GENERATOR.with_borrow_mut(|generator| generator.generate());

        self.symbol_map
            .insert(identifier.name.as_str(), random_name.clone());
        identifier.name = self.ast_builder.atom(&random_name);

        walk_binding_identifier(self, identifier);
    }

    fn visit_identifier_reference(&mut self, identifier: &mut IdentifierReference<'a>) {
        if let Some(name) = self.symbol_map.get(identifier.name.as_str()) {
            identifier.name = self.ast_builder.atom(name);
        }

        walk_identifier_reference(self, identifier);
    }
}
