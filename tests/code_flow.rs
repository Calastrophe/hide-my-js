use hide_my_js::passes::control_flow::ControlFlowFlattener;
use hide_my_js::passes::renaming::Renamer;
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

        function fn2() {
            function hello() {
                console.log("hello again");
                console.log("hello again");
                console.log("hello again");
            }

            let i = 0;
            while (i < 5) {
                if (i >= 2) {
                    if (i == 3)
                        break;
                } else {
                    if (1) {
                        i++;
                        continue;
                    }
                }
                console.log("loop it:", i);
                i++;
            }

            console.log("firstline");
            console.log("secondline");
            let num = 5;
            console.log("thirdline", num);
            hello();
        }
        console.log(hello_str());
        fn2();
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

    let mut transformer = ControlFlowFlattener::new(&ast_builder);
    let mut renamer = Renamer::new(&ast_builder);
    transformer.visit_program(&mut program);
    println!("{program:?}");
    renamer.visit_program(&mut program);

    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);
}
