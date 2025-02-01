use oxc::ast::ast::{BindingIdentifier, IdentifierReference};
use oxc::ast::{AstBuilder, VisitMut};
use rand::Rng;
use rand::distr::Alphanumeric;

struct VariableRenamer<'a> {
    ast_builder: &'a AstBuilder<'a>,
    variable_map: std::collections::HashMap<String, String>,
}

impl<'a> VariableRenamer<'a> {
    fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            variable_map: std::collections::HashMap::new(),
        }
    }

    fn generate_random_name(&self) -> String {
        let mut rng = rand::rng();
        let length = rng.random_range(8..15);
        let random_part: String = rng
            .sample_iter(&Alphanumeric)
            .take(length)
            .map(char::from)
            .collect();
        format!("_{}", random_part)
    }
}

impl<'a> VisitMut<'a> for VariableRenamer<'a> {
    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        // replace the identifier
    }

    fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
        // replace the identifier reference
    }

    // TODO: fn visit_identifier_name ... determine if this is needed
}
