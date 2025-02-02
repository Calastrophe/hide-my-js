use crate::utils;
use oxc::ast::ast::{Expression, Function, FunctionBody, Statement, VariableDeclarationKind};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::semantic::ScopeFlags;

pub struct DeadCodeInserter<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> DeadCodeInserter<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    fn insert_dead_code(&mut self, function: &mut Function<'a>) {
        // Insert random variables that are calculated from any variables present in the function
        // These random varialbes are useless and just contribute to expressions that are also useless.
        // We also copy statements from the original function and put them behind an opaque if statement
        // This opaque if statement incorporates these random variables, but it always evaluate to false
    }
}

impl<'a> VisitMut<'a> for DeadCodeInserter<'a> {
    fn visit_function(&mut self, function: &mut Function<'a>, _flags: ScopeFlags) {
        self.insert_dead_code(function);
    }
}
