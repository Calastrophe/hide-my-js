use hide_my_js::passes::renaming::Renamer;
use oxc::ast::AstBuilder;
use oxc::ast::VisitMut;
use oxc::codegen::Codegen;
use oxc::parser::{Parser, ParserReturn};
use oxc::semantic::SemanticBuilder;
use oxc::semantic::SemanticBuilderReturn;
use oxc::span::SourceType;

#[test]
fn variable_renaming() {
    let code = r#"
        function factorial(n) {
            if (n === 0 || n === 1) return 1;
            return n * factorial(n - 1);
        }

        function fibonacci(n) {
            if (n <= 1) return n;
            return fibonacci(n - 1) + fibonacci(n - 2);
        }

        function sumArray(arr) {
            return arr.reduce((acc, val) => acc + val, 0);
        }

        function productArray(arr) {
            return arr.reduce((acc, val) => acc * val, 1);
        }

        function gcd(a, b) {
            if (b === 0) return a;
            return gcd(b, a % b);
        }

        function lcm(a, b) {
            return (a * b) / gcd(a, b);
        }

        function power(base, exponent) {
            if (exponent === 0) return 1;
            return base * power(base, exponent - 1);
        }

        function circleArea(radius) {
            return Math.PI * Math.pow(radius, 2);
        }

        console.log("Factorial of 5:", factorial(5)); // 120
        console.log("Fibonacci of 6:", fibonacci(6)); // 8
        console.log("Sum of [1, 2, 3, 4]:", sumArray([1, 2, 3, 4])); // 10
        console.log("Product of [1, 2, 3, 4]:", productArray([1, 2, 3, 4])); // 24
        console.log("GCD of 12 and 18:", gcd(12, 18)); // 6
        console.log("LCM of 12 and 18:", lcm(12, 18)); // 36
        console.log("2^5:", power(2, 5)); // 32
        console.log("Area of circle with radius 3:", circleArea(3)); // ~28.274
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

    let mut transformer = Renamer::new(&ast_builder);
    transformer.visit_program(&mut program);

    let obfuscated_code = Codegen::new().build(&program);

    println!("Obfuscated code:\n{}", obfuscated_code.code);
}
