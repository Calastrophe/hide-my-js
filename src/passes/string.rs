use oxc::ast::ast::{Argument, BindingIdentifier, CallExpression, Expression, IdentifierReference, TSTypeParameterInstantiation};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::span::{Atom, Span};
use base64::prelude::*;

pub struct StringEncoder<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> StringEncoder<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
        }
    }

    fn encode(&self, string: &str) -> String {
        BASE64_STANDARD.encode(string)
    }
}

impl<'a> VisitMut<'a> for StringEncoder<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        match it {
            Expression::StringLiteral(string_literal) => {
                let encoded = self.encode(*&string_literal.value.to_string().as_str());
                let allocated = self.ast_builder.allocator.alloc_str(&encoded);
                *string_literal = self.ast_builder.alloc_string_literal(string_literal.span, Atom::from(&*allocated), None);
            },
            Expression::Identifier(identifier) =>{
                *it = Expression::CallExpression(self.ast_builder.alloc_call_expression(identifier.span, Expression::Identifier(self.ast_builder.alloc_identifier_reference(identifier.span, "atob")), None::<oxc::allocator::Box<TSTypeParameterInstantiation>>, self.ast_builder.vec_from_array([Argument::Identifier(oxc::allocator::Box::new_in(identifier.to_owned(), &self.ast_builder.allocator))]), false));
            }
            _ => {}
        }
    }
}
