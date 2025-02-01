use oxc::ast::ast::{BindingIdentifier, BindingPatternKind, Expression, IdentifierReference};
use oxc::ast::{
    AstBuilder, VisitMut,
    visit::walk_mut::{walk_binding_pattern_kind, walk_expression},
};
use rand::Rng;
use rand::distr::Alphanumeric;

pub struct VariableRenamer<'a> {
    ast_builder: &'a AstBuilder<'a>,
    variable_map: std::collections::HashMap<String, String>,
}

impl<'a> VariableRenamer<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
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
    fn visit_binding_pattern_kind(&mut self, pattern: &mut BindingPatternKind<'a>) {
        if let BindingPatternKind::BindingIdentifier(identifier) = pattern {
            let name = self.generate_random_name();
            self.variable_map
                .insert(identifier.name.into_string(), name.clone());
            *identifier = self
                .ast_builder
                .alloc_binding_identifier(identifier.span, name);
        }

        walk_binding_pattern_kind(self, pattern);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        if let Expression::Identifier(ident_ref) = expr {
            if let Some(name) = self.variable_map.get(ident_ref.name.as_str()) {
                *ident_ref = self
                    .ast_builder
                    .alloc_identifier_reference(ident_ref.span, name)
            }
        }

        walk_expression(self, expr);
    }
    // TODO: fn visit_identifier_name ... determine if this is needed
}
