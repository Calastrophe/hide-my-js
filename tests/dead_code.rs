use hide_my_js::passes::dead_code::DeadCodeInserter;
use hide_my_js::passes::renaming::Renamer;
use oxc::ast::AstBuilder;
use oxc::ast::VisitMut;
use oxc::codegen::Codegen;
use oxc::parser::{Parser, ParserReturn};
use oxc::semantic::SemanticBuilder;
use oxc::semantic::SemanticBuilderReturn;
use oxc::span::SourceType;

#[test]
fn dead_code() {
    let code = r#"
        function calculateRandomStuff1() {
            let a = 10;
            let b = 25;
            let c = 42;
            let d = 7;
            let e = 15;
            let f = 33;
            let g = 56;
            let h = 89;
            let i = 123;
            let j = 456;

            let result1 = a + b * c - d / e + f % g;
            let result2 = (h * i) / j + (a - b) * (c + d);
            let result3 = Math.pow(e, 2) + Math.sqrt(f) - Math.abs(g - h);
            let result4 = (i + j) * (a - b) / (c + d) - (e * f);

            let temp1 = result1 + result2;
            let temp2 = result3 - result4;
            let temp3 = temp1 * temp2;
            let temp4 = temp3 / (a + b + c);

            let finalResult1 = temp4 + (d * e) - (f / g) + (h % i);

            console.log("Final Result 1:", finalResult1);

            // More random calculations
            let x = 100;
            let y = 200;
            let z = 300;
            let w = 400;

            let result5 = x + y - z * w / x;
            let result6 = Math.pow(y, 2) - Math.sqrt(z) + Math.abs(w - x);
            let result7 = (x * y) / (z + w) - (x % y);

            let temp5 = result5 + result6;
            let temp6 = result7 - temp5;
            let temp7 = temp6 * (x + y + z);

            let finalResult2 = temp7 / (w - x) + (y % z);

            console.log("Final Result 2:", finalResult2);

            return finalResult1 + finalResult2;
        }
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

    let mut transformer = DeadCodeInserter::new(&ast_builder);
    let mut renamer = Renamer::new(&ast_builder);
    transformer.visit_program(&mut program);
    //renamer.visit_program(&mut program);

    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);
}
