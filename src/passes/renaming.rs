use oxc::ast::ast::{Atom, BindingIdentifier, IdentifierReference};
use oxc::ast::visit::walk_mut::{walk_binding_identifier, walk_identifier_reference};
use oxc::ast::{AstBuilder, VisitMut};
use rand::distr::Alphanumeric;
use rand::Rng;
use std::collections::HashMap;

pub struct Renamer<'a> {
    ast_builder: &'a AstBuilder<'a>,
    symbol_map: HashMap<&'a str, String>,
}

fn generate_random_name() -> String {
    let mut rng = rand::rng();
    let length = rng.random_range(8..15);
    let random_part: String = rng
        .sample_iter(&Alphanumeric)
        .take(length)
        .map(char::from)
        .collect();
    format!("_{}", random_part)
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
        let random_name = generate_random_name();
        let ident_name = self.ast_builder.allocator.alloc_str(&random_name);
        self.symbol_map
            .insert(identifier.name.as_str(), random_name);
        identifier.name = Atom::from(&*ident_name);

        walk_binding_identifier(self, identifier);
    }

    fn visit_identifier_reference(&mut self, identifier: &mut IdentifierReference<'a>) {
        if let Some(name) = self.symbol_map.get(identifier.name.as_str()) {
            let name = self.ast_builder.allocator.alloc_str(name);
            identifier.name = Atom::from(&*name);
        }

        walk_identifier_reference(self, identifier);
    }
}
