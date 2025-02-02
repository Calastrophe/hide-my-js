use std::fs::File;
use std::io::Write;

use hide_my_js::passes::renaming::Renamer;
use oxc::ast::AstBuilder;
use oxc::ast::VisitMut;
use oxc::codegen::Codegen;
use oxc::parser::{Parser, ParserReturn};
use oxc::semantic::SemanticBuilder;
use oxc::semantic::SemanticBuilderReturn;
use oxc::span::SourceType;


#[test]
fn test_all_passes() { 
    let mut code = //this is blocking, we really dont need to get a full async stack here to download one file 
        reqwest::blocking::get("https://raw.githubusercontent.com/tj/commander.js/refs/heads/master/lib/command.js").unwrap().text().unwrap();

    let allocator = oxc::allocator::Allocator::default();
    let mut errors = Vec::new();
    let ParserReturn {
        mut program,
        errors: parser_errors,
        ..
    } = Parser::new(&allocator, &code, SourceType::cjs()).parse();
    errors.extend(parser_errors);

    let SemanticBuilderReturn {
        semantic: _,
        errors: semantic_errors,
    } = SemanticBuilder::new()
        .with_check_syntax_error(true)
        .build(&program);
    errors.extend(semantic_errors);

    let ast_builder = AstBuilder::new(&allocator);

    let mut transformer = Renamer::new(&ast_builder);
    transformer.visit_program(&mut program);

    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);





    let mut file = File::create("test_output.temp").unwrap();
    file.write_all(obfuscated_code.code.as_bytes()).unwrap();


    }
