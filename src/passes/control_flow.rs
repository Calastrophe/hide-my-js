use core::panic;

use oxc::allocator::CloneIn;
use oxc::ast::ast::{AssignmentOperator, BlockStatement, ExpressionStatement, FunctionBody, TSTypeAnnotation, VariableDeclarationKind};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::ast::ast::Expression;
use oxc::span::Atom;
use rand::seq::SliceRandom;
use rand::Rng;
use oxc::ast::ast::BigintBase;
use oxc::ast::ast::Statement;

use crate::utils;

pub struct ControlFlowFlattener<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> ControlFlowFlattener<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
        }
    }

    fn generate_case_number(&self) -> i32 {
        let mut rng = rand::rng();
        return rng.gen_range(-1000..=1000);
    }

    fn flatten_func_body(&mut self, it: &mut FunctionBody<'a>) {
        let mut switch_cases = self.ast_builder.vec();

        let mut raw_case_num = self.generate_case_number();

        let (state_var_declaration, state_identifier) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, it.span, "state", raw_case_num);

        let (inf_loop_cond, inf_loop_cond_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, it.span, "loop_cond", 1);

        //let block_statement = self.ast_builder.alloc_block_statement(it.span, body);
        //let while_block = self.ast_builder.alloc_while_statement(it.span, Expression::BooleanLiteral(self.ast_builder.alloc_boolean_literal(it.span, true)), )
        let mut stmnt_cnt = 0;
        let total_stmnts = it.statements.len();

        let mut body_statements = self.ast_builder.vec();

        for statement in &mut it.statements {
            let mut moved_statement = self.ast_builder.move_statement(statement);

            let mut is_ret = false;

            match &mut moved_statement {
                Statement::ReturnStatement(_) => {
                    is_ret = true;
                },
                Statement::WhileStatement(while_stmnt) => {
                    
                },
                Statement::FunctionDeclaration(func) => {
                    if let Some(fn_body) = &mut func.body {
                        self.flatten_func_body(fn_body);
                    }

                    body_statements.push(moved_statement);
                    stmnt_cnt += 1;
                    continue;
                },
                Statement::ClassDeclaration(_) | Statement::VariableDeclaration(_) => {
                    body_statements.push(moved_statement);
                    stmnt_cnt += 1;
                    continue;
                },
                _ => {},
            }

            let mut case_block = self.ast_builder.vec_from_array([moved_statement]);

            let cur_case_num = raw_case_num;

            raw_case_num = self.generate_case_number();

            if stmnt_cnt != total_stmnts - 1 {
                let expression_statement = self.ast_builder.alloc_expression_statement(it.span, Expression::AssignmentExpression(utils::create_assignment_expression(self.ast_builder, it.span, state_identifier.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, it.span, raw_case_num, BigintBase::Decimal)))));
                case_block.push(Statement::ExpressionStatement(expression_statement));
            } else {
                if !is_ret {
                    // add assignment statement for setting conditional to 0
                    let expression_statement = self.ast_builder.alloc_expression_statement(it.span, Expression::AssignmentExpression(utils::create_assignment_expression(self.ast_builder, it.span, inf_loop_cond_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, it.span, 0, BigintBase::Decimal)))));
                    case_block.push(Statement::ExpressionStatement(expression_statement));
                }
            }

            // adding break statement
            case_block.push(Statement::BreakStatement(self.ast_builder.alloc_break_statement(it.span, None)));

            // appending case to switch statement

            let case_statement = self.ast_builder.switch_case(it.span, Some(Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, it.span, cur_case_num, BigintBase::Decimal))), case_block);

            switch_cases.push(case_statement);

            stmnt_cnt += 1;
        }

        // shuffle cases to ranndomize order
        switch_cases.shuffle(&mut rand::rng());

        let block_switch = self.ast_builder.alloc_switch_statement(it.span, Expression::Identifier(state_identifier),switch_cases);

        let while_loop = self.ast_builder.alloc_while_statement(it.span, Expression::Identifier(inf_loop_cond_id), Statement::SwitchStatement(block_switch));

        body_statements.push(Statement::VariableDeclaration(state_var_declaration));
        body_statements.push(Statement::WhileStatement(while_loop));
        //let body_statements = self.ast_builder.vec_from_array([Statement::VariableDeclaration(state_var_declaration), Statement::WhileStatement(while_loop)]);

        *it = self.ast_builder.function_body(it.span, self.ast_builder.move_vec(&mut it.directives), body_statements);
    }
}

impl<'a> VisitMut<'a> for ControlFlowFlattener<'a> {
    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.flatten_func_body(it);
    }
}
