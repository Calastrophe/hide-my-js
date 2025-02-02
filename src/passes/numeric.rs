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
        let rand_val = rng.random_range(0.0..value);

        // If it isn't a whole number, we can just use addition and subtraction.
        if value.fract() != 0.0 {
            let a = self.ast_builder.expression_numeric_literal(
                literal.span,
                rand_val,
                None,
                literal.base,
            );

            let (value, operator) = if rng.random_bool(0.5) {
                (
                    literal.value - rand_val,
                    oxc::ast::ast::BinaryOperator::Addition,
                )
            } else {
                (
                    literal.value + rand_val,
                    oxc::ast::ast::BinaryOperator::Subtraction,
                )
            };

            let b = self.ast_builder.expression_numeric_literal(
                literal.span,
                value,
                None,
                literal.base,
            );

            self.ast_builder.expression_parenthesized(
                literal.span,
                self.ast_builder
                    .expression_binary(literal.span, a, operator, b),
            )
        } else {
            // If its a whole number, lets just use XOR and multiplication.
            let a = value as i32;

            let (left_value, right_value, operator) = if rng.random_bool(0.80) {
                let b = rng.random::<i32>();
                let c = a ^ b;
                (
                    b as f64,
                    c as f64,
                    oxc::ast::ast::BinaryOperator::BitwiseXOR,
                )
            } else {
                let b = rng.random_range(1..=10);
                let c = a * b;
                (c as f64, b as f64, oxc::ast::ast::BinaryOperator::Division)
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
                        right_value,
                        None,
                        literal.base,
                    ),
                ),
            )
        }
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
