use std::fs::File;
use std::io::Write;

use hide_my_js::passes::control_flow::ControlFlowFlattener;
use hide_my_js::passes::dead_code::DeadCodeInserter;
use hide_my_js::passes::numeric::NumericObfuscation;
use hide_my_js::passes::remove_comments::RemoveComments;
use hide_my_js::passes::renaming::Renamer;
use hide_my_js::passes::string::StringEncoder;
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

        
    let mut control_flow_flattener = ControlFlowFlattener::new(&ast_builder);
    control_flow_flattener.visit_program(&mut program);

    let mut dead_code_inserter = DeadCodeInserter::new(&ast_builder);
    dead_code_inserter.visit_program(&mut program);

    let mut numeric_obfuscation = NumericObfuscation::new(&ast_builder);
    numeric_obfuscation.visit_program(&mut program);

    let mut remove_comments = RemoveComments::new(&ast_builder);
    remove_comments.visit_program(&mut program); 

    let mut renamer = Renamer::new(&ast_builder);
    renamer.visit_program(&mut program);

    let mut string_encoder = StringEncoder::new(&ast_builder);
    string_encoder.visit_program(&mut program);


    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);





    let mut file = File::create("test_output.temp.js").unwrap();
    file.write_all(obfuscated_code.code.as_bytes()).unwrap();


    }
