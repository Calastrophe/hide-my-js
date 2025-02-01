use oxc::allocator::CloneIn;
use oxc::ast::ast::{Argument, BindingPatternKind, Expression, Statement};
use oxc::ast::visit::walk_mut::walk_statement;
use oxc::ast::{
    AstBuilder, VisitMut,
    visit::walk_mut::{walk_binding_pattern_kind, walk_expression},
};
use rand::Rng;
use rand::distr::Alphanumeric;

pub struct Renamer<'a> {
    ast_builder: &'a AstBuilder<'a>,
    variable_map: std::collections::HashMap<String, String>,
    function_map: std::collections::HashMap<String, String>,
}

impl<'a> Renamer<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            variable_map: std::collections::HashMap::new(),
            function_map: std::collections::HashMap::new(),
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

impl<'a> VisitMut<'a> for Renamer<'a> {
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

    fn visit_statement(&mut self, stmt: &mut Statement<'a>) {
        if let Statement::FunctionDeclaration(function) = stmt {
            if let Some(identifier) = function.name() {
                let random_name = self.generate_random_name();
                let name = self
                    .function_map
                    .entry(identifier.into_string())
                    .or_insert(random_name);
                let ident = self
                    .ast_builder
                    .binding_identifier(function.id.as_ref().unwrap().span, name.as_str());
                *function = self.ast_builder.alloc_function(
                    function.span,
                    function.r#type,
                    Some(ident),
                    function.generator,
                    function.r#async,
                    function.declare,
                    function
                        .type_parameters
                        .clone_in(&self.ast_builder.allocator),
                    function.this_param.clone_in(&self.ast_builder.allocator),
                    function.params.clone_in(&self.ast_builder.allocator),
                    function.return_type.clone_in(&self.ast_builder.allocator),
                    function.body.clone_in(&self.ast_builder.allocator),
                )
            }
        }

        walk_statement(self, stmt);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        match expr {
            Expression::FunctionExpression(function) => {
                if let Some(identifier) = function.name() {
                    let random_name = self.generate_random_name();
                    let name = self
                        .function_map
                        .entry(identifier.into_string())
                        .or_insert(random_name);
                    let ident = self
                        .ast_builder
                        .binding_identifier(function.id.as_ref().unwrap().span, name.as_str());
                    *function = self.ast_builder.alloc_function(
                        function.span,
                        function.r#type,
                        Some(ident),
                        function.generator,
                        function.r#async,
                        function.declare,
                        function
                            .type_parameters
                            .clone_in(&self.ast_builder.allocator),
                        function.this_param.clone_in(&self.ast_builder.allocator),
                        function.params.clone_in(&self.ast_builder.allocator),
                        function.return_type.clone_in(&self.ast_builder.allocator),
                        function.body.clone_in(&self.ast_builder.allocator),
                    );
                }
            }
            Expression::Identifier(ident_ref) => {
                if let Some(name) = self.variable_map.get(ident_ref.name.as_str()) {
                    *ident_ref = self
                        .ast_builder
                        .alloc_identifier_reference(ident_ref.span, name);
                }

                if let Some(name) = self.function_map.get(ident_ref.name.as_str()) {
                    *ident_ref = self
                        .ast_builder
                        .alloc_identifier_reference(ident_ref.span, name);
                }
            }
            _ => {}
        }

        walk_expression(self, expr);
    }
}
