use crate::utils;
use oxc::allocator::CloneIn;
use oxc::ast::ast::{Expression, Function, FunctionBody, Statement, VariableDeclarationKind};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::semantic::ScopeFlags;
use rand::Rng;

pub struct DeadCodeInserter<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> DeadCodeInserter<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    fn insert_dead_code(&mut self, body: &mut FunctionBody<'a>) {
        let mut rng = rand::rng();
        let mut new_statements = self.ast_builder.vec();

        let mut useless_variables = vec![];
        let mut control_variables = vec![];
        let guaranteed_control = rand::random_range(0..4);
        for i in 0..rand::random_range(4..=7) {
            let var_name = utils::generate_random_name();
            let is_control = rand::random_bool(0.10) || i == guaranteed_control;
            let value = if i == guaranteed_control {
                rng.random_range(i32::MIN..-15000)
            } else {
                rng.random_range(-10000..=10000)
            };
            let (var_decl, var_ident) = utils::create_var_i32(
                self.ast_builder,
                VariableDeclarationKind::Let,
                body.span,
                &var_name,
                value,
            );
            if !is_control {
                useless_variables.push(var_ident);
            } else {
                control_variables.push(var_ident)
            };
            new_statements.push(Statement::VariableDeclaration(var_decl));
        }

        for statement in &mut body.statements {
            if rng.random_bool(0.3) {
                let c_idx = rng.random_range(0..control_variables.len());
                let u_idx = rng.random_range(0..useless_variables.len());
                let (lhs, op, rhs) = if rng.random_bool(0.5) {
                    (
                        Expression::Identifier(
                            useless_variables[u_idx].clone_in(self.ast_builder.allocator),
                        ),
                        oxc::ast::ast::BinaryOperator::GreaterThan,
                        Expression::Identifier(
                            control_variables[c_idx].clone_in(self.ast_builder.allocator),
                        ),
                    )
                } else {
                    (
                        Expression::Identifier(
                            control_variables[c_idx].clone_in(self.ast_builder.allocator),
                        ),
                        oxc::ast::ast::BinaryOperator::LessThan,
                        Expression::Identifier(
                            useless_variables[u_idx].clone_in(self.ast_builder.allocator),
                        ),
                    )
                };
                let opaque_condition = self
                    .ast_builder
                    .alloc_binary_expression(body.span, lhs, op, rhs);
                let if_statement = self.ast_builder.alloc_if_statement(
                    body.span,
                    Expression::BinaryExpression(opaque_condition),
                    self.ast_builder.move_statement(statement),
                    None,
                );
                new_statements.push(Statement::IfStatement(if_statement));
            } else {
                new_statements.push(self.ast_builder.move_statement(statement));
            }
        }

        body.statements = new_statements;
    }
}

impl<'a> VisitMut<'a> for DeadCodeInserter<'a> {
    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        self.insert_dead_code(body);
    }
}
