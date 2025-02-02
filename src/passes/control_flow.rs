use core::panic;

use oxc::allocator::{Box, CloneIn};
use oxc::ast::ast::{AssignmentOperator, BlockStatement, BreakStatement, ExpressionStatement, ForStatementInit, FunctionBody, IdentifierReference, IfStatement, SwitchCase, TSTypeAnnotation, VariableDeclarationKind, WhileStatement};
use oxc::ast::{AstBuilder, VisitMut};
use oxc::ast::ast::Expression;
use oxc::span::{Atom, Span};
use rand::distr::Alphanumeric;
use rand::seq::SliceRandom;
use rand::Rng;
use oxc::ast::ast::BigintBase;
use oxc::ast::ast::Statement;

use crate::utils;

struct OuterLoop<'a> {
    ast_builder: &'a AstBuilder<'a>,
    condition: oxc::allocator::Box<'a, IdentifierReference<'a>>,
    state_id: oxc::allocator::Box<'a, IdentifierReference<'a>>,
    exit_case_nums: Option<(i32, i32)>,
    initial_case_num: i32,
}

impl<'a> OuterLoop<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>, condition: oxc::allocator::Box<'a, IdentifierReference<'a>>, state_id: oxc::allocator::Box<'a, IdentifierReference<'a>>, exit_case_nums: Option<(i32, i32)>, initial_case_num: i32) -> Self {
        Self { ast_builder, condition, state_id, exit_case_nums, initial_case_num }
    }
}

impl<'a> Clone for OuterLoop<'a> {
    fn clone(&self) -> Self {
        Self { ast_builder: self.ast_builder, condition: self.condition.clone_in(&self.ast_builder.allocator), state_id: self.state_id.clone_in(&self.ast_builder.allocator), exit_case_nums: self.exit_case_nums, initial_case_num: self.initial_case_num }
    }
}

pub struct ControlFlowFlattener<'a> {
    ast_builder: &'a AstBuilder<'a>,
}

impl<'a> ControlFlowFlattener<'a> {
    pub fn new(ast_builder: &'a AstBuilder<'a>) -> Self {
        Self {
            ast_builder,
        }
    }

    fn generate_random_name() -> String {
        let mut rng = rand::rng();
        let length = rng.random_range(8..15);
        let random_part: String = rng
            .sample_iter(&Alphanumeric)
            .take(length)
            .map(char::from)
            .collect();
        format!("_{}", random_part)
    }

    fn generate_case_number(&self) -> i32 {
        let mut rng = rand::rng();
        return rng.gen_range(std::i32::MIN..=std::i32::MAX);
    }

    fn create_initial_loop_case(&self, span: Span, case_number: i32, state_var: oxc::allocator::Box<'a, IdentifierReference<'a>>, new_condition: Box<'a, IdentifierReference<'a>>, real_condition: Expression<'a>) -> (i32, i32, SwitchCase<'a>) {
        let next_case_number = self.generate_case_number();

        let goto_next_case_assignment = utils::create_assignment_expression(self.ast_builder, span, state_var.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, next_case_number, BigintBase::Decimal)));
        let end_loop_assignment = utils::create_assignment_expression(self.ast_builder, span, new_condition.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, 0, BigintBase::Decimal)));

        let goto_next_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(goto_next_case_assignment)));
        let end_loop_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(end_loop_assignment)));

        let break_statement = self.ast_builder.alloc_break_statement(span, None);

        let if_statement = self.ast_builder.alloc_if_statement(span, real_condition, goto_next_statement, Some(end_loop_statement));

        let initial_case_literal = utils::create_big_int_literal(self.ast_builder, span, case_number, BigintBase::Decimal);
        let initial_case = self.ast_builder.switch_case(span, Some(Expression::BigIntLiteral(initial_case_literal)), self.ast_builder.vec_from_array([Statement::IfStatement(if_statement), Statement::BreakStatement(break_statement)]));

        // return case_number, next_case_number, case
        (case_number, next_case_number, initial_case)
    }

    fn create_break_loop_case(&self, span: Span, case_number: i32, inner_loop: OuterLoop<'a>, outer_loop: Option<OuterLoop<'a>>) -> SwitchCase<'a> {
        let case_number_literal = utils::create_big_int_literal(self.ast_builder, span, case_number, BigintBase::Decimal);

        let mut case_statements = self.ast_builder.vec();

        let zero_number_literal = utils::create_big_int_literal(self.ast_builder, span, 0, BigintBase::Decimal);

        let inner_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, inner_loop.condition.name, AssignmentOperator::Assign, Expression::BigIntLiteral(zero_number_literal.clone_in(&self.ast_builder.allocator)));
        let inner_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(inner_cond_assignment_expression)));

        case_statements.push(inner_cond_assignment_statement);

        if let Some(outer_loop) = outer_loop {
            if let Some((break_case_num, _)) = outer_loop.exit_case_nums {
                let initial_case_literal = utils::create_big_int_literal(self.ast_builder, span, break_case_num, BigintBase::Decimal);

                let outer_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, outer_loop.state_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(initial_case_literal));
                let outer_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(outer_cond_assignment_expression)));

                case_statements.push(outer_cond_assignment_statement);
            } else {
                let outer_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, outer_loop.condition.name, AssignmentOperator::Assign, Expression::BigIntLiteral(zero_number_literal));
                let outer_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(outer_cond_assignment_expression)));

                case_statements.push(outer_cond_assignment_statement);
            }
        }

        case_statements.push(Statement::BreakStatement(self.ast_builder.alloc_break_statement(span, None)));

        self.ast_builder.switch_case(span, Some(Expression::BigIntLiteral(case_number_literal)), case_statements)
    }

    fn create_continue_loop_case(&self, span: Span, case_number: i32, inner_loop: OuterLoop<'a>, outer_loop: Option<OuterLoop<'a>>) -> SwitchCase<'a> {
        let case_number_literal = utils::create_big_int_literal(self.ast_builder, span, case_number, BigintBase::Decimal);

        let mut case_statements = self.ast_builder.vec();

        let zero_number_literal = utils::create_big_int_literal(self.ast_builder, span, 0, BigintBase::Decimal);

        let inner_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, inner_loop.condition.name, AssignmentOperator::Assign, Expression::BigIntLiteral(zero_number_literal));
        let inner_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(inner_cond_assignment_expression)));

        case_statements.push(inner_cond_assignment_statement);

        if let Some(outer_loop) = outer_loop {
            if let Some((_, continue_case_num))= outer_loop.exit_case_nums {
                let initial_case_literal = utils::create_big_int_literal(self.ast_builder, span, continue_case_num, BigintBase::Decimal);

                let outer_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, outer_loop.state_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(initial_case_literal));
                let outer_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(outer_cond_assignment_expression)));
       
                case_statements.push(outer_cond_assignment_statement);
            } else {
                let initial_case_literal = utils::create_big_int_literal(self.ast_builder, span, outer_loop.initial_case_num, BigintBase::Decimal);

                let outer_cond_assignment_expression = utils::create_assignment_expression(self.ast_builder, span, outer_loop.state_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(initial_case_literal));
                let outer_cond_assignment_statement = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(outer_cond_assignment_expression)));

                case_statements.push(outer_cond_assignment_statement);
            }
        }

        case_statements.push(Statement::BreakStatement(self.ast_builder.alloc_break_statement(span, None)));

        self.ast_builder.switch_case(span, Some(Expression::BigIntLiteral(case_number_literal)), case_statements)
    }

    fn flatten_statement(&mut self, span: Span, statement: &mut Statement<'a>, outer_loop: Option<OuterLoop<'a>>) {
        match statement {
            Statement::BreakStatement(break_stmnt) => {
                if let Some(outer_loop) = outer_loop {
                    if let Some((break_case_num, _)) = outer_loop.exit_case_nums {
                        let mut block_statements = self.ast_builder.vec();
    
                        // set outer loop state to break case
                        let goto_break_assign_expr = utils::create_assignment_expression(self.ast_builder, span, outer_loop.state_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, break_case_num, BigintBase::Decimal)));
                        let goto_break_assign_stmnt = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(goto_break_assign_expr)));

                        block_statements.push(goto_break_assign_stmnt);
                        block_statements.push(Statement::BreakStatement(break_stmnt.clone_in(&self.ast_builder.allocator)));
    
                        *statement = Statement::BlockStatement(self.ast_builder.alloc_block_statement(break_stmnt.span, block_statements));
                    }
                }
            },
            Statement::ContinueStatement(continue_stmnt) => {
                if let Some(outer_loop) = outer_loop {
                    if let Some((_, continue_case_num)) = outer_loop.exit_case_nums {
                        let mut block_statements = self.ast_builder.vec();
    
                        // set outer loop state to break case
                        let goto_continue_assign_expr = utils::create_assignment_expression(self.ast_builder, span, outer_loop.state_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, continue_case_num, BigintBase::Decimal)));
                        let goto_continue_assign_stmnt = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(goto_continue_assign_expr)));

                        block_statements.push(goto_continue_assign_stmnt);
                        block_statements.push(Statement::BreakStatement(self.ast_builder.alloc_break_statement(continue_stmnt.span, None)));
    
                        *statement = Statement::BlockStatement(self.ast_builder.alloc_block_statement(continue_stmnt.span, block_statements));
                    }
                }
            },
            Statement::IfStatement(if_stmnt) => {
                let span = if_stmnt.span;

                self.flatten_statement(span, &mut if_stmnt.consequent, outer_loop.clone());

                if let Some(alternate) = &mut if_stmnt.alternate {
                    self.flatten_statement(span, alternate, outer_loop.clone());
                }
            },
            Statement::WhileStatement(while_stmnt) => {
                let original_test = while_stmnt.test.clone_in(&self.ast_builder.allocator);
                let (new_test_var, new_test_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, while_stmnt.span, &Self::generate_random_name(), 1);

                // lets create a block that wraps everything first so we can put the new_var = original_var outside it

                let mut block_statements = self.ast_builder.vec();
                
                block_statements.push(Statement::VariableDeclaration(new_test_var));
                
                // create initial case and create an outer_loop object for it

                let initial_case_number = self.generate_case_number();

                let (state_var, state_var_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, while_stmnt.span, &Self::generate_random_name(), initial_case_number);

                let (initial_case_number, cur_case_number, initial_case) = self.create_initial_loop_case(while_stmnt.span, initial_case_number, state_var_id.clone_in(&self.ast_builder.allocator), new_test_id.clone_in(&self.ast_builder.allocator), original_test);

                block_statements.push(Statement::VariableDeclaration(state_var));

                let outer_loop = OuterLoop::new(self.ast_builder, new_test_id.clone_in(&self.ast_builder.allocator), state_var_id.clone_in(&self.ast_builder.allocator), None, initial_case_number);
                
                // then flatten the body and put it in the next case. when flattening body be sure to pass the Some(outer_loop) object
                self.flatten_statement(while_stmnt.span, &mut while_stmnt.body, Some(outer_loop.clone()));

                let mut body_case_statements = self.ast_builder.vec();

                body_case_statements.push(while_stmnt.body.clone_in(&self.ast_builder.allocator));

                let goto_initial_assign_expr = utils::create_assignment_expression(self.ast_builder, while_stmnt.span, state_var_id.name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, while_stmnt.span, initial_case_number, BigintBase::Decimal)));
                let goto_initial_assign_stmnt = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(goto_initial_assign_expr)));

                let if_continue_stmnt = Statement::IfStatement(self.ast_builder.alloc_if_statement(span, Expression::Identifier(new_test_id.clone_in(&self.ast_builder.allocator)), goto_initial_assign_stmnt, None));

                body_case_statements.push(if_continue_stmnt);

                body_case_statements.push(Statement::BreakStatement(self.ast_builder.alloc_break_statement(while_stmnt.span, None)));

                let body_case = self.ast_builder.switch_case(while_stmnt.span, Some(Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, while_stmnt.span, cur_case_number, BigintBase::Decimal))), body_case_statements);
                
                let mut cases = self.ast_builder.vec_from_array([initial_case, body_case]);

                cases.shuffle(&mut rand::rng());

                let switch_statement = self.ast_builder.alloc_switch_statement(while_stmnt.span, Expression::Identifier(state_var_id), cases);

                let new_while = self.ast_builder.alloc_while_statement(while_stmnt.span, Expression::Identifier(new_test_id), Statement::SwitchStatement(switch_statement));
            
                block_statements.push(Statement::WhileStatement(new_while));

                *statement = Statement::BlockStatement(self.ast_builder.alloc_block_statement(span, block_statements));
            },
            Statement::BlockStatement(block) => {
                // for each statement first attempt to flatten it and then just put the flattened statement in a case!
                let span = block.span;

                let (cond_var, cond_var_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, span, &Self::generate_random_name(), 1);

                let mut cur_case_num = self.generate_case_number();

                let break_case_num = self.generate_case_number();
                let continue_case_num = self.generate_case_number();

                let (state_var, state_var_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, span, &Self::generate_random_name(), cur_case_num);

                let inner_loop = OuterLoop::new(self.ast_builder, cond_var_id.clone_in(&self.ast_builder.allocator), state_var_id.clone_in(&self.ast_builder.allocator), Some((break_case_num, continue_case_num)), cur_case_num);

                let break_case = self.create_break_loop_case(span, break_case_num, inner_loop.clone(), outer_loop.clone());
                let contiue_case = self.create_continue_loop_case(span, continue_case_num, inner_loop.clone(), outer_loop.clone());

                let mut block_statements = self.ast_builder.vec();
                let mut switch_cases = self.ast_builder.vec();

                switch_cases.push(break_case);
                switch_cases.push(contiue_case);

                block_statements.push(Statement::VariableDeclaration(cond_var));
                block_statements.push(Statement::VariableDeclaration(state_var));

                let mut stmnt_idx = 0;
                let total_stmnts = block.body.len();

                for statement in &mut block.body {
                    // first attempt to flatten before creating case for statement
                    self.flatten_statement(span, statement, Some(inner_loop.clone()));

                    if let Statement::VariableDeclaration(var_decl) = statement {
                        block_statements.push(Statement::VariableDeclaration(var_decl.clone_in(&self.ast_builder.allocator)));
                        stmnt_idx += 1;
                        continue;
                    }

                    // create case
                    let real_case_number = cur_case_num;
                    let case_number_literal = utils::create_big_int_literal(self.ast_builder, span, cur_case_num, BigintBase::Decimal);
                    
                    cur_case_num = self.generate_case_number();

                    let mut case_statements = self.ast_builder.vec();

                    case_statements.push(statement.clone_in(&self.ast_builder.allocator));

                    let break_statement = Statement::BreakStatement(self.ast_builder.alloc_break_statement(span, None));

                    let assign_statement;

                    if stmnt_idx == total_stmnts - 1 {
                        let break_inner_loop_assin = utils::create_assignment_expression(self.ast_builder, span, cond_var_id.clone_in(&self.ast_builder.allocator).name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, 0, BigintBase::Decimal)));
                        let break_inner_loop_stmnt = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(break_inner_loop_assin)));

                        assign_statement = break_inner_loop_stmnt;
                    } else {
                        let goto_next_assign_expr = utils::create_assignment_expression(self.ast_builder, span, state_var_id.clone_in(&self.ast_builder.allocator).name, AssignmentOperator::Assign, Expression::BigIntLiteral(utils::create_big_int_literal(self.ast_builder, span, cur_case_num, BigintBase::Decimal)));
                        let goto_next_assign_stmnt = Statement::ExpressionStatement(self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(goto_next_assign_expr)));

                        assign_statement = goto_next_assign_stmnt;
                    }

                    let (assign_check_expr_var, assign_check_expr_id) = utils::create_var_i32(self.ast_builder, VariableDeclarationKind::Let, span, &Self::generate_random_name(), real_case_number);
                    let assign_check_expr_init = utils::create_assignment_expression(self.ast_builder, span, assign_check_expr_id.name, AssignmentOperator::BitwiseXOR, Expression::Identifier(state_var_id.clone_in(&self.ast_builder.allocator)));

                    let check_expr = self.ast_builder.alloc_expression_statement(span, Expression::AssignmentExpression(assign_check_expr_init));

                    case_statements.push(Statement::VariableDeclaration(assign_check_expr_var));
                    case_statements.push(Statement::ExpressionStatement(check_expr));

                    let if_stmnt = Statement::IfStatement(self.ast_builder.alloc_if_statement(span, Expression::Identifier(assign_check_expr_id), Statement::EmptyStatement(self.ast_builder.alloc_empty_statement(span)), Some(assign_statement)));

                    case_statements.push(if_stmnt);    

                    case_statements.push(break_statement);

                    let switch_case = self.ast_builder.switch_case(span, Some(Expression::BigIntLiteral(case_number_literal)), case_statements);

                    switch_cases.push(switch_case);

                    stmnt_idx += 1;
                }

                switch_cases.shuffle(&mut rand::rng());

                let switch = self.ast_builder.alloc_switch_statement(span, Expression::Identifier(state_var_id), switch_cases);

                let while_loop = self.ast_builder.alloc_while_statement(span, Expression::Identifier(cond_var_id), Statement::SwitchStatement(switch));

                block_statements.push(Statement::WhileStatement(while_loop));

                block.body = block_statements;
            },
            _ => {}
        }
    }
}

impl<'a> VisitMut<'a> for ControlFlowFlattener<'a> {
    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let statements_copy = it.statements.clone_in(&self.ast_builder.allocator);
        let mut new_block = Statement::BlockStatement(self.ast_builder.alloc_block_statement(it.span, statements_copy));

        self.flatten_statement(it.span, &mut new_block, None);

        if let Statement::BlockStatement(block) = new_block {
            it.statements = self.ast_builder.vec_from_array([Statement::BlockStatement(block)]);
        }
    }
}
