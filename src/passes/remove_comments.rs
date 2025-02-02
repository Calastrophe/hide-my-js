use oxc::ast::{AstBuilder, VisitMut};


pub struct RemoveComments<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> RemoveComments<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }
}

impl<'a> VisitMut<'a> for RemoveComments<'a> { 
    fn visit_program(&mut self, program: &mut oxc::ast::ast::Program<'a>) {
        program.comments.clear();
    }
}

