use oxc::{
    ast::ast::{BinaryOperator, Expression, FunctionBody, Statement},
    ast_visit::{walk_mut, VisitMut},
};
use rand::Rng;

use super::{number, Context};

/// Insert pure, unreachable branches without wrapping live statements.
pub(super) struct DeadCode<'ctx, 'a> {
    /// The run's random stream and arena.
    context: &'ctx mut Context<'a>,
}

impl<'ctx, 'a> DeadCode<'ctx, 'a> {
    /// Borrow the current transformation run.
    pub(super) const fn new(context: &'ctx mut Context<'a>) -> Self {
        Self { context }
    }
}

impl<'a> VisitMut<'a> for DeadCode<'_, 'a> {
    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        walk_mut::walk_function_body(self, body);
        let ast = &self.context.ast;
        let span = body.span;
        let value = self.context.rng.random_range(1..=i32::MAX);
        // No declarations or user-defined operations, even in unreachable code.
        let condition = Expression::new_binary_expression(
            span,
            number(ast, span, value),
            BinaryOperator::StrictInequality,
            number(ast, span, value),
            ast,
        );
        let noise = Expression::new_binary_expression(
            span,
            number(ast, span, value),
            BinaryOperator::BitwiseXOR,
            number(ast, span, !value),
            ast,
        );
        let block = Statement::new_block_statement(
            span,
            [Statement::new_expression_statement(span, noise, ast)],
            ast,
        );
        body.statements.insert(
            0,
            Statement::new_if_statement(span, condition, block, None, ast),
        );
    }
}
