use core::{alloc, num};

use oxc::{
    allocator::Allocator,
    ast::{
        ast::{
            BigintBase, BinaryExpression, BinaryOperator, Expression, ExpressionStatement, IfStatement, NumericLiteral, Statement
        },
        visit::VisitMut,
        AstBuilder,
    },
    span::Span,
};
use rand::Rng;

use crate::utils::generate_random_name;

/// Struct for injecting opaque predicates
pub struct OpaquePredicateInjection<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> OpaquePredicateInjection<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }


    fn generate_random_num(&self) -> i32 {
        let mut rng = rand::rng();
        return rng.random_range(-1000..=1000);
    }

    /// Inserts an opaque predicate into the AST
    pub fn inject_opaque_predicate(&self, statements: Statement<'a>) {
        let allocator = self.ast_builder.allocator;

        let random_number = generate_random_name();
        let random_number_float = random_number.clone().parse::<f64>().unwrap(); //we can unwrap here its probably not going to error

        let predicate =BinaryExpression { 
            span: Default::default(),
            left: Expression::NumericLiteral(oxc::allocator::Box::new_in(
                NumericLiteral {
                    span: Span::default(),
                    value: random_number_float,
                    raw: Some(random_number.as_str().into()),
                    base: oxc::ast::ast::NumberBase::Decimal,
                },
                allocator,
            )),
            operator: BinaryOperator::BitwiseXOR,
            right: Expression::NumericLiteral(oxc::allocator::Box::new_in(
                NumericLiteral {
                    span: Span::default(),
                    value: 123.0,
                    raw: Some("123".into()),
                    base: oxc::ast::ast::NumberBase::Decimal,
                },
                allocator,
            )),
        };
        
        let if_statement = Statement::IfStatement(
            oxc::allocator::Box::new_in(
                IfStatement {
                    span: Span::default(),
                    test: Expression::BinaryExpression(oxc::allocator::Box::new_in(predicate, allocator)), // Remove the extra Box::new_in
                    consequent: statements,
                    alternate: None,
                },
                allocator,)
        );
  
    }
}


impl<'a> VisitMut<'a> for OpaquePredicateInjection<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) { //visit expr might be incorrect
        todo!();
        //For Ian to impl
    }
}
