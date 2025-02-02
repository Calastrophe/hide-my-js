pub mod passes;
pub mod utils;

use self::passes::control_flow::ControlFlowFlattener;
use self::passes::dead_code::DeadCodeInserter;
use self::passes::numeric::NumericObfuscation;
use self::passes::remove_comments::RemoveComments;
use self::passes::renaming::Renamer;
use self::passes::string::StringEncoder;
use oxc::ast::AstBuilder;
use oxc::ast::VisitMut;
use oxc::codegen::Codegen;
use oxc::parser::{Parser, ParserReturn};
use oxc::semantic::SemanticBuilder;
use oxc::semantic::SemanticBuilderReturn;
use oxc::span::SourceType;



pub fn obfuscate_code(code: String,
    control_flow: bool,
    dead_code: bool,
    numeric: bool,
    remove_comments: bool,
    renaming: bool,
    string: bool,
) -> String { 

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
    
        if control_flow { 
            let mut control_flow_flattener = ControlFlowFlattener::new(&ast_builder);
            control_flow_flattener.visit_program(&mut program);
        }

        if dead_code  { 
            let mut dead_code_inserter = DeadCodeInserter::new(&ast_builder);
            dead_code_inserter.visit_program(&mut program);
        }

        if numeric { 
            let mut numeric_obfuscation = NumericObfuscation::new(&ast_builder);
            numeric_obfuscation.visit_program(&mut program);
        }

        if remove_comments { 
            let mut remove_comments = RemoveComments::new(&ast_builder);
            remove_comments.visit_program(&mut program); 
        }

        if renaming { 
            let mut renamer = Renamer::new(&ast_builder);
            renamer.visit_program(&mut program);
        }

        if string { 
            let mut string_encoder = StringEncoder::new(&ast_builder);
            string_encoder.visit_program(&mut program);
        }

        let obfuscated_code = Codegen::new().build(&program).code;
        obfuscated_code
}
