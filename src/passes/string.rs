use std::collections::HashSet;

use base64::prelude::*;
use oxc::ast::ast::{Argument, Expression, TSTypeParameterInstantiation, VariableDeclarator};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::span::Atom;

pub struct StringEncoder<'a> {
    ast_builder: &'a AstBuilder<'a>,
    string_vars: HashSet<String>,
}

impl<'a> StringEncoder<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
            string_vars: HashSet::new(),
        }
    }

    fn encode(&self, string: &str) -> String {
        BASE64_STANDARD.encode(string)
    }
}

impl<'a> VisitMut<'a> for StringEncoder<'a> {
    fn visit_variable_declarator(&mut self, var: &mut VariableDeclarator<'a>) {
        if let Some(expression) = &mut var.init {
            if let Expression::StringLiteral(string_literal) = expression {
                if let Some(name) = var.id.kind.get_identifier() {
                    let allocated_str = self
                        .ast_builder
                        .allocator
                        .alloc_str(&self.encode(&string_literal.value));
                    string_literal.value = Atom::from(&*allocated_str);

                    self.string_vars.insert(name.into_string());
                }
            }
        }
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        match it {
            Expression::Identifier(identifier) => {
                if self.string_vars.contains(&identifier.name.to_string()) {
                    *it = Expression::CallExpression(
                        self.ast_builder.alloc_call_expression(
                            identifier.span,
                            Expression::Identifier(
                                self.ast_builder
                                    .alloc_identifier_reference(identifier.span, "atob"),
                            ),
                            None::<oxc::allocator::Box<TSTypeParameterInstantiation>>,
                            self.ast_builder.vec_from_array([Argument::Identifier(
                                oxc::allocator::Box::new_in(
                                    identifier.to_owned(),
                                    &self.ast_builder.allocator,
                                ),
                            )]),
                            false,
                        ),
                    );
                }
            }
            _ => {}
        }
    }
}
