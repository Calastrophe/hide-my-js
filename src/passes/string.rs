use oxc::ast::ast::StringLiteral;
use oxc::ast::{AstBuilder, VisitMut};

pub struct StringEncoder<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> StringEncoder<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self { ast_builder }
    }

    fn to_escape_bytes(&self, str: &str) -> String {
        str.as_bytes()
            .iter()
            .map(|b| format!(r"\x{:02x}", b))
            .collect()
    }
}

impl<'a> VisitMut<'a> for StringEncoder<'a> {
    fn visit_string_literal(&mut self, literal: &mut StringLiteral<'a>) {
        let allocated_string = self.ast_builder.atom(&self.to_escape_bytes(&literal.value));
        literal.value = allocated_string;
    }
}
