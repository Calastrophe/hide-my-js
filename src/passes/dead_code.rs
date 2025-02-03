use crate::utils;
use oxc::allocator::CloneIn;
use oxc::ast::ast::{Expression, FunctionBody, Statement, VariableDeclarationKind};
use oxc::ast::visit::walk_mut::walk_function_body;
use oxc::ast::{AstBuilder, VisitMut};
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

        if rand::random_bool(0.60) {
            return;
        }

        let mut new_statements = self.ast_builder.vec();

        let mut useless_variables = vec![];
        let mut control_variables = vec![];
        let guaranteed_control = rand::random_range(0..4);
        for i in 0..rand::random_range(4..=7) {
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
                value,
            );
            if !is_control {
                useless_variables.push(var_ident);
            } else {
                control_variables.push(var_ident)
            };
            new_statements.push(Statement::VariableDeclaration(var_decl));
        }

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

        let mut block_statements = self.ast_builder.vec();
        for stmt in &mut body.statements {
            block_statements.push(self.ast_builder.move_statement(stmt));
        }

        let block_statement = self
            .ast_builder
            .alloc_block_statement(body.span, block_statements);

        let if_statement = self.ast_builder.alloc_if_statement(
            body.span,
            Expression::BinaryExpression(opaque_condition),
            Statement::BlockStatement(block_statement),
            None,
        );
        new_statements.push(Statement::IfStatement(if_statement));

        body.statements = new_statements;
    }
}

impl<'a> VisitMut<'a> for DeadCodeInserter<'a> {
    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        self.insert_dead_code(body);

        walk_function_body(self, body);
    }
}
