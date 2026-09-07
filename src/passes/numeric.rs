use oxc::{
    ast::ast::{BinaryOperator, Expression, NumericLiteral, PropertyKey},
    ast_visit::{walk_mut, VisitMut},
};
use rand::Rng;

use super::{number, Context};

/// Mask int32 literals with XOR, retaining other numbers exactly.
pub(super) struct Numeric<'ctx, 'a> {
    /// The run's random stream and arena.
    context: &'ctx mut Context<'a>,
}

impl<'ctx, 'a> Numeric<'ctx, 'a> {
    /// Borrow the current transformation run.
    pub(super) const fn new(context: &'ctx mut Context<'a>) -> Self {
        Self { context }
    }

    /// Return a mask only if int32 coercion preserves the exact JavaScript value.
    fn mask(&mut self, literal: &NumericLiteral<'a>) -> Option<Expression<'a>> {
        let value = literal.value;
        if value.fract() != 0.0
            || value < f64::from(i32::MIN)
            || value > f64::from(i32::MAX)
            || (value == 0.0 && value.is_sign_negative())
        {
            return None;
        }
        #[expect(
            clippy::cast_possible_truncation,
            clippy::as_conversions,
            reason = "range and integrality checked before conversion"
        )]
        let value = value as i32;
        let ast = &self.context.ast;
        let mask = self.context.rng.random_range(i32::MIN..=i32::MAX);
        Some(Expression::new_binary_expression(
            literal.span,
            number(ast, literal.span, value ^ mask),
            BinaryOperator::BitwiseXOR,
            number(ast, literal.span, mask),
            ast,
        ))
    }
}

impl<'a> VisitMut<'a> for Numeric<'_, 'a> {
    fn visit_property_key(&mut self, key: &mut PropertyKey<'a>) {
        if !matches!(key, PropertyKey::NumericLiteral(_)) {
            walk_mut::walk_property_key(self, key);
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression<'a>) {
        if let Expression::NumericLiteral(literal) = expression {
            if let Some(masked) = self.mask(literal) {
                *expression = masked;
            }
        } else {
            walk_mut::walk_expression(self, expression);
        }
    }
}
