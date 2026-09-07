use std::collections::HashSet;

use oxc::{
    allocator::Allocator,
    ast::ast::{
        BinaryExpression, BinaryOperator, BindingIdentifier, ContinueStatement, DoWhileStatement,
        IfStatement, LabeledStatement, ObjectExpression, StringLiteral, SwitchStatement,
        TemplateElement,
    },
    ast_visit::{walk, Visit},
    parser::Parser,
    span::SourceType,
};

/// Observable output features, independent of formatting and randomized spellings.
#[derive(Default)]
#[expect(
    clippy::redundant_pub_crate,
    reason = "helpers are shared only within this integration test"
)]
pub(crate) struct Structure {
    /// Original or generated binding spellings.
    pub(crate) bindings: HashSet<String>,
    /// Literal and cooked template values, so escaping alone cannot hide plaintext.
    pub(crate) strings: HashSet<String>,
    /// Case counts for each emitted switch, used to enforce dispatcher bounds.
    pub(crate) switches: Vec<usize>,
    /// Object sizes; dedicated pool fixtures contain no user object expressions.
    pub(crate) objects: Vec<usize>,
    /// Conditional statements still outside a switch dispatcher.
    pub(crate) structured_branches: usize,
    /// Labels which have not been resolved to CFG edges.
    pub(crate) labels: usize,
    /// Continues which have not been resolved to CFG edges.
    pub(crate) continues: usize,
    /// Original do/while loops; neither dispatcher layout uses this form.
    pub(crate) do_loops: usize,
    /// XOR operations, used to detect masking in an otherwise XOR-free fixture.
    pub(crate) xors: usize,
    /// Current switch nesting depth during inspection.
    switch_depth: usize,
}

impl Structure {
    /// Parse generated script syntax solely to inspect transformation evidence.
    ///
    /// # Panics
    /// Fails if the generated code cannot be parsed.
    pub(crate) fn read(source: &str) -> Self {
        let allocator = Allocator::default();
        let parsed = Parser::new(&allocator, source, SourceType::cjs()).parse();
        assert!(
            parsed.diagnostics.is_empty(),
            "invalid generated syntax: {:?}",
            parsed.diagnostics
        );
        let mut structure = Self::default();
        structure.visit_program(&parsed.program);
        structure
    }
}

impl<'a> Visit<'a> for Structure {
    fn visit_binding_identifier(&mut self, binding: &BindingIdentifier<'a>) {
        self.bindings.insert(binding.name.to_string());
    }
    fn visit_string_literal(&mut self, literal: &StringLiteral<'a>) {
        self.strings.insert(literal.value.to_string());
    }
    fn visit_template_element(&mut self, element: &TemplateElement<'a>) {
        if let Some(value) = element.value.cooked {
            self.strings.insert(value.to_string());
        }
    }
    fn visit_switch_statement(&mut self, switch: &SwitchStatement<'a>) {
        self.switches.push(switch.cases.len());
        self.switch_depth += 1;
        walk::walk_switch_statement(self, switch);
        self.switch_depth -= 1;
    }
    fn visit_if_statement(&mut self, branch: &IfStatement<'a>) {
        if self.switch_depth == 0 {
            self.structured_branches += 1;
        }
        walk::walk_if_statement(self, branch);
    }
    fn visit_labeled_statement(&mut self, label: &LabeledStatement<'a>) {
        self.labels += 1;
        walk::walk_labeled_statement(self, label);
    }
    fn visit_continue_statement(&mut self, _: &ContinueStatement<'a>) {
        self.continues += 1;
    }
    fn visit_do_while_statement(&mut self, loop_: &DoWhileStatement<'a>) {
        self.do_loops += 1;
        walk::walk_do_while_statement(self, loop_);
    }
    fn visit_binary_expression(&mut self, binary: &BinaryExpression<'a>) {
        if binary.operator == BinaryOperator::BitwiseXOR {
            self.xors += 1;
        }
        walk::walk_binary_expression(self, binary);
    }
    fn visit_object_expression(&mut self, object: &ObjectExpression<'a>) {
        self.objects.push(object.properties.len());
        walk::walk_object_expression(self, object);
    }
}
