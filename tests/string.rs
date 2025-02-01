use hide_my_js::passes::renaming::VariableRenamer;
use hide_my_js::passes::string::StringEncoder;
use oxc::ast::AstBuilder;
use oxc::ast::VisitMut;
use oxc::codegen::Codegen;
use oxc::parser::{Parser, ParserReturn};
use oxc::semantic::SemanticBuilder;
use oxc::semantic::SemanticBuilderReturn;
use oxc::span::SourceType;

#[test]
fn string() {
    let code = r#"
        function hello_str() {
            let str = "hello world";
            return str;
        }
        console.log(hello_str());
    "#;

    let allocator = oxc::allocator::Allocator::default();
    let mut errors = Vec::new();
    let ParserReturn {
        mut program,
        errors: parser_errors,
        ..
    } = Parser::new(&allocator, code, SourceType::cjs()).parse();
    errors.extend(parser_errors);

    let SemanticBuilderReturn {
        semantic: _,
        errors: semantic_errors,
    } = SemanticBuilder::new()
        .with_check_syntax_error(true)
        .build(&program);
    errors.extend(semantic_errors);

    let ast_builder = AstBuilder::new(&allocator);

    let mut transformer = StringEncoder::new(&ast_builder);
    transformer.visit_program(&mut program);

    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);
}
