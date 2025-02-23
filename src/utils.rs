use oxc::allocator::Box;
use oxc::{
    ast::{
        ast::{
            AssignmentExpression, AssignmentOperator, BigIntLiteral, BigintBase, Expression,
            IdentifierReference, TSTypeAnnotation, VariableDeclaration, VariableDeclarationKind,
        },
        AstBuilder,
    },
    span::{Atom, Span},
};

pub mod identifier;

pub fn create_var_i32<'a>(
    ast_builder: &'a AstBuilder,
    kind: VariableDeclarationKind,
    span: Span,
    initial_value: i32,
) -> (
    Box<'a, VariableDeclaration<'a>>,
    Box<'a, IdentifierReference<'a>>,
) {
    let name = identifier::GENERATOR.with_borrow_mut(|generator| generator.generate());
    let var_binding_id = ast_builder.alloc_binding_identifier(span, name);
    let name = var_binding_id.name.to_owned();
    let var_binding_pattern = ast_builder.binding_pattern(
        oxc::ast::ast::BindingPatternKind::BindingIdentifier(var_binding_id),
        None::<oxc::allocator::Box<TSTypeAnnotation>>,
        false,
    );

    let init_num = ast_builder.alloc_big_int_literal(
        span,
        ast_builder.atom(&initial_value.to_string()),
        BigintBase::Decimal,
    );

    let var_declarator = ast_builder.variable_declarator(
        span,
        kind,
        var_binding_pattern,
        Some(Expression::BigIntLiteral(init_num)),
        false,
    );

    let var_declaration = ast_builder.alloc_variable_declaration(
        span,
        kind,
        ast_builder.vec_from_array([var_declarator]),
        false,
    );

    (
        var_declaration,
        ast_builder.alloc_identifier_reference(span, name),
    )
}

pub fn create_big_int_literal<'a>(
    ast_builder: &'a AstBuilder,
    span: Span,
    inital_value: i32,
    base: BigintBase,
) -> Box<'a, BigIntLiteral<'a>> {
    ast_builder.alloc_big_int_literal(span, ast_builder.atom(&inital_value.to_string()), base)
}

pub fn create_assignment_expression<'a>(
    ast_builder: &'a AstBuilder,
    span: Span,
    var_name: Atom<'a>,
    operator: AssignmentOperator,
    right_expression: Expression<'a>,
) -> Box<'a, AssignmentExpression<'a>> {
    let assignment_target =
        ast_builder.simple_assignment_target_identifier_reference(span, var_name);
    ast_builder.alloc_assignment_expression(
        span,
        operator,
        assignment_target.into(),
        right_expression,
    )
}
