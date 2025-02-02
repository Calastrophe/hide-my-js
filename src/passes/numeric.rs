use oxc::ast::{
    ast::{Expression, NumericLiteral},
    visit::walk_mut::walk_expression,
    AstBuilder, VisitMut,
};
use rand::Rng;

pub struct NumericObfuscation<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> NumericObfuscation<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    pub fn generate_equivalent_expression(
        &self,
        literal: &oxc::allocator::Box<'a, NumericLiteral>,
    ) -> Expression<'a> {
        let mut rng = rand::rng();
        let value = literal.value;
        let right_value = rng.random_range(i32::MIN..=i32::MAX);

        let upper_bound = if value.fract() != 0.0 { 1 } else { 2 };
        let (left_value, operator) = match rng.random_range(0..=upper_bound) {
            0 => (
                value - right_value as f64,
                oxc::ast::ast::BinaryOperator::Addition,
            ),
            1 => (
                value + right_value as f64,
                oxc::ast::ast::BinaryOperator::Subtraction,
            ),
            2 => (
                (value as i32 ^ right_value) as f64,
                oxc::ast::ast::BinaryOperator::BitwiseXOR,
            ),
            _ => unreachable!(),
        };

        self.ast_builder.expression_parenthesized(
            literal.span,
            self.ast_builder.expression_binary(
                literal.span,
                self.ast_builder.expression_numeric_literal(
                    literal.span,
                    left_value,
                    None,
                    literal.base,
                ),
                operator,
                self.ast_builder.expression_numeric_literal(
                    literal.span,
                    right_value as f64,
                    None,
                    literal.base,
                ),
            ),
        )
    }
}

impl<'a> VisitMut<'a> for NumericObfuscation<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        match expr {
            Expression::NumericLiteral(literal) => {
                *expr = self.generate_equivalent_expression(literal);
                return;
            }
            _ => {}
        }

        walk_expression(self, expr);
    }
}
