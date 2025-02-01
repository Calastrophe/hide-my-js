use oxc::ast::ast::{BindingPatternKind, Expression, Function, Statement};
use oxc::ast::visit::walk_mut::walk_statement;
use oxc::ast::{
    visit::walk_mut::{walk_binding_pattern_kind, walk_expression},
    AstBuilder, VisitMut,
};
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

    fn rename_function(&mut self, function: &mut oxc::allocator::Box<'a, Function<'a>>) {
        if let Some(identifier) = function.name() {
            let name = self
                .symbol_map
                .entry(identifier.as_str())
                .or_insert_with(|| generate_random_name());
            let ident = self
                .ast_builder
                .binding_identifier(function.id.as_ref().unwrap().span, name.as_str());
            function.id = Some(ident);
        }
    }
}

impl<'a> VisitMut<'a> for Renamer<'a> {
    fn visit_binding_pattern_kind(&mut self, pattern: &mut BindingPatternKind<'a>) {
        if let BindingPatternKind::BindingIdentifier(identifier) = pattern {
            let name = generate_random_name();
            self.symbol_map
                .insert(identifier.name.as_str(), name.clone());
            *identifier = self
                .ast_builder
                .alloc_binding_identifier(identifier.span, name);
        }

        walk_binding_pattern_kind(self, pattern);
    }

    fn visit_statement(&mut self, stmt: &mut Statement<'a>) {
        if let Statement::FunctionDeclaration(function) = stmt {
            self.rename_function(function);
        }

        walk_statement(self, stmt);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        match expr {
            Expression::FunctionExpression(function) => self.rename_function(function),
            Expression::Identifier(ident_ref) => {
                if let Some(name) = self.symbol_map.get(ident_ref.name.as_str()) {
                    *ident_ref = self
                        .ast_builder
                        .alloc_identifier_reference(ident_ref.span, name)
                }
            }
            _ => {}
        }

        walk_expression(self, expr);
    }
}
