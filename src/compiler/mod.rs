pub mod symbol_table;

use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::code;
use crate::code::{Bytecode, CompiledFunction, Constant, Instructions, OpCode};
pub use crate::compiler::symbol_table::{Symbol, SymbolScope, SymbolTable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::rc::Rc;

const TENTATIVE_JUMP_POS: u16 = 9999;

pub struct Compiler {
    pub constants: Rc<RefCell<Vec<Constant>>>,
    symbol_table: Rc<RefCell<SymbolTable>>,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Default for Compiler {
    fn default() -> Self {
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new_with_builtins()));

        let constants = Rc::new(RefCell::new(vec![]));

        Compiler::new_with_state(symbol_table, constants)
    }
}

impl Compiler {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_state(
        symbol_table: Rc<RefCell<SymbolTable>>,
        constants: Rc<RefCell<Vec<Constant>>>,
    ) -> Self {
        let main_scope = CompilationScope::new();

        Compiler {
            constants,
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn compile(mut self, program: &Program) -> Result<Bytecode, CompileError> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }
        Ok(self.bytecode())
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Expression(exp) => {
                self.compile_expression(exp)?;
                self.emit(OpCode::Pop);
            }
            Statement::Let(name, exp) => {
                // Define the symbol first so that recursive functions can reference themselves in
                // their function bodies.
                let symbol = *self.symbol_table.borrow_mut().define(name);

                self.compile_expression(exp)?;

                match symbol.scope {
                    SymbolScope::Global => {
                        self.emit_with_operands(OpCode::SetGlobal, OpCode::u16(symbol.index));
                    }
                    SymbolScope::Local => {
                        self.emit_with_operands(OpCode::SetLocal, vec![symbol.index as u8]);
                    }
                    SymbolScope::Free => {
                        panic!("free cannot be defined by let statement");
                    }
                    SymbolScope::Builtin => {
                        panic!("builtin cannot be defined by let statement");
                    }
                }
            }
            Statement::Return(None) => {
                self.emit(OpCode::Return);
            }
            Statement::Return(Some(value)) => {
                self.compile_expression(value)?;
                self.emit(OpCode::ReturnValue);
            }
        }
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Infix(infix, left, right) => {
                match infix {
                    Infix::Lt => {
                        // Convert `a < b` to `a > b` to keep the instruction set smaller.
                        // TODO: This is an issue if expressions have side effects though.
                        self.compile_expression(right)?;
                        self.compile_expression(left)?;
                    }
                    _ => {
                        self.compile_expression(left)?;
                        self.compile_expression(right)?;
                    }
                }
                match infix {
                    Infix::Plus => {
                        self.emit(OpCode::Add);
                    }
                    Infix::Minus => {
                        self.emit(OpCode::Sub);
                    }
                    Infix::Asterisk => {
                        self.emit(OpCode::Mul);
                    }
                    Infix::Slash => {
                        self.emit(OpCode::Div);
                    }
                    Infix::Eq => {
                        self.emit(OpCode::Equal);
                    }
                    Infix::NotEq => {
                        self.emit(OpCode::NotEqual);
                    }
                    Infix::Gt | Infix::Lt => {
                        self.emit(OpCode::GreaterThan);
                    }
                }
            }
            Expression::Prefix(prefix, right) => {
                self.compile_expression(right)?;

                match prefix {
                    Prefix::Bang => {
                        self.emit(OpCode::Bang);
                    }
                    Prefix::Minus => {
                        self.emit(OpCode::Minus);
                    }
                }
            }
            Expression::IntegerLiteral(value) => {
                let constant = Constant::Integer(*value);
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            Expression::Boolean(true) => {
                self.emit(OpCode::True);
            }
            Expression::Boolean(false) => {
                self.emit(OpCode::False);
            }
            Expression::StringLiteral(value) => {
                let constant = Constant::String(value.clone());
                let const_index = self.add_constant(constant)?;
                self.emit_with_operands(OpCode::Constant, OpCode::u16(const_index));
            }
            // if (condition) { consequence }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. push null
            // 6. pop
            //
            // --
            //
            // if (condition) { consequence } else { alternative }
            //
            // should emit:
            //
            // 1. condition
            // 2. jump to 5 if not truthy
            // 3. consequence (without the last pop)
            // 4. jump to 6
            // 5. alternative (without the last pop)
            // 6. pop
            Expression::If(condition, consequence, alternative) => {
                self.compile_expression(condition)?;
                let jump_not_truthy_pos = self
                    .emit_with_operands(OpCode::JumpIfNotTruthy, OpCode::u16(TENTATIVE_JUMP_POS));

                self.compile_block_statement(consequence)?;
                if self.last_instruction_is(OpCode::Pop) {
                    self.remove_last_pop();
                }

                let jump_pos =
                    self.emit_with_operands(OpCode::Jump, OpCode::u16(TENTATIVE_JUMP_POS));

                self.replace_instruction(
                    jump_not_truthy_pos,
                    code::make_u16(
                        OpCode::JumpIfNotTruthy,
                        self.current_instructions().len() as u16,
                    ),
                );

                match alternative {
                    Some(alt) => {
                        self.compile_block_statement(alt)?;
                        if self.last_instruction_is(OpCode::Pop) {
                            self.remove_last_pop();
                        }
                    }
                    None => {
                        self.emit(OpCode::Null);
                    }
                }

                self.replace_instruction(
                    jump_pos,
                    code::make_u16(OpCode::Jump, self.current_instructions().len() as u16),
                );
            }
            Expression::Identifier(name) => {
                let symbol = {
                    match self.symbol_table.borrow_mut().resolve(name) {
                        Some(symbol) => symbol,
                        None => return Err(CompileError::UndefinedVariable(name.to_string())),
                    }
                };
                self.load_symbol(symbol);
            }
            Expression::Array(exps) => {
                for exp in exps {
                    self.compile_expression(exp)?;
                }
                self.emit_with_operands(OpCode::Array, OpCode::u16(exps.len() as u16));
            }
            Expression::Hash(hash) => {
                let size = hash.pairs.len();
                // Have a stable order of hash pairs
                let mut str_keys = Vec::with_capacity(size);
                let mut map = HashMap::with_capacity(size);
                for (key, value) in &hash.pairs {
                    // Using `String` as key because it's hard to implement
                    // `PartialOrd` for `Expression`.
                    let str_key = key.to_string();
                    // TODO: Is it possible to do this without cloning?
                    str_keys.push(str_key.clone());
                    map.insert(str_key, (key, value));
                }
                str_keys.sort();

                for str_key in str_keys {
                    let (key, value) = map.get(&str_key).unwrap();
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }
                self.emit_with_operands(OpCode::Hash, OpCode::u16(size as u16));
            }
            Expression::Index(left, index) => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;
                self.emit(OpCode::Index);
            }
            Expression::FunctionLiteral(params, body) => {
                let num_params = params.len();
                if num_params > 0xff {
                    return Err(CompileError::TooManyParams);
                }

                self.enter_scope();

                for param in params {
                    self.symbol_table.borrow_mut().define(param);
                }

                self.compile_block_statement(body)?;
                // Take care of implicit return like `fn() { 5 }`
                if self.last_instruction_is(OpCode::Pop) {
                    self.replace_last_pop_with_return();
                }
                // Take care of empty body like `fn() { }`
                if !self.last_instruction_is(OpCode::ReturnValue) {
                    self.emit(OpCode::Return);
                }

                let num_locals = self.symbol_table.borrow().num_definitions();
                if num_locals > 0xff {
                    return Err(CompileError::TooManyLocals);
                }
                let (instructions, free_symbols) = self.leave_scope();

                let num_frees = free_symbols.len();
                if num_frees > 0xff {
                    return Err(CompileError::TooManyFrees);
                }

                // Load free variables before the OpCode::Closure so that the VM can build a closure
                // with free variables.
                for free in free_symbols {
                    self.load_symbol(free);
                }

                let compiled_function = Constant::CompiledFunction(CompiledFunction {
                    instructions,
                    num_locals: num_locals as u8,
                    num_parameters: num_params as u8,
                });
                let const_index = self.add_constant(compiled_function)?;
                self.emit_with_operands(
                    OpCode::Closure,
                    OpCode::u16_u8(const_index, num_frees as u8),
                );
            }
            Expression::Call(func, args) => {
                self.compile_expression(func)?;
                for arg in args {
                    self.compile_expression(arg)?;
                }
                self.emit_with_operands(OpCode::Call, vec![args.len() as u8]);
            }
        }
        Ok(())
    }

    fn compile_block_statement(
        &mut self,
        block_statement: &BlockStatement,
    ) -> Result<(), CompileError> {
        for statement in &block_statement.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();
        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table.borrow_mut().push();
    }

    fn leave_scope(&mut self) -> (Instructions, Vec<Symbol>) {
        let scope = self.scopes.pop().expect("no scope to leave from");
        self.scope_index -= 1;

        let free_symbols = self.symbol_table.borrow_mut().pop();

        (scope.instructions, free_symbols)
    }

    fn add_constant(&mut self, constant: Constant) -> Result<u16, CompileError> {
        let constant_index = self.constants.borrow().len();
        if constant_index >= 0xffff {
            return Err(CompileError::TooManyConstants);
        }
        self.constants.borrow_mut().push(constant);
        Ok(constant_index as u16)
    }

    fn emit(&mut self, op_code: OpCode) -> usize {
        // TODO: Isn't it slow to create `vec![]` each time?
        self.emit_with_operands(op_code, vec![])
    }

    fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        self.scopes[self.scope_index].emit_with_operands(op_code, operands)
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    fn replace_instruction(&mut self, pos: usize, instruction: Instructions) {
        self.scopes[self.scope_index].replace_instruction(pos, instruction)
    }

    fn last_instruction_is(&self, op_code: OpCode) -> bool {
        self.scopes[self.scope_index].last_instruction_is(op_code)
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].remove_last_pop()
    }

    fn replace_last_pop_with_return(&mut self) {
        self.scopes[self.scope_index].replace_last_pop_with_return()
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Builtin => {
                self.emit_with_operands(OpCode::GetBuiltin, vec![symbol.index as u8]);
            }
            SymbolScope::Global => {
                self.emit_with_operands(OpCode::GetGlobal, OpCode::u16(symbol.index));
            }
            SymbolScope::Free => {
                self.emit_with_operands(OpCode::GetFree, vec![symbol.index as u8]);
            }
            SymbolScope::Local => {
                self.emit_with_operands(OpCode::GetLocal, vec![symbol.index as u8]);
            }
        }
    }

    fn bytecode(&self) -> Bytecode {
        let scope = &self.scopes[self.scope_index];
        // TODO: Can't this be done without cloning? Compiler's ownership moves to Bytecode anyway...
        Bytecode::new(scope.instructions.clone(), self.constants.borrow().clone())
    }
}

pub struct EmittedInstruction {
    pub op_code: OpCode,
    pub position: usize,
}

#[derive(Default)]
pub struct CompilationScope {
    pub instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn emit_with_operands(&mut self, op_code: OpCode, operands: Vec<u8>) -> usize {
        let pos = self.instructions.len();
        self.instructions.push(op_code as u8);
        self.instructions.extend(operands);
        self.set_last_instruction(op_code, pos);
        pos
    }

    // Not updating last/previous_instruction because we still don't have cases
    // that require it.
    pub fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, byte) in new_instruction.iter().enumerate() {
            let offset = pos + i;
            if offset < self.instructions.len() {
                self.instructions[offset] = *byte;
            } else {
                self.instructions.push(*byte);
            }
        }
    }

    fn set_last_instruction(&mut self, op_code: OpCode, position: usize) {
        self.previous_instruction = mem::replace(
            &mut self.last_instruction,
            Some(EmittedInstruction { op_code, position }),
        );
    }

    pub fn last_instruction_is(&self, op_code: OpCode) -> bool {
        match &self.last_instruction {
            Some(emitted) => emitted.op_code == op_code,
            None => false,
        }
    }

    pub fn remove_last_pop(&mut self) {
        if let Some(emitted) = &self.last_instruction {
            self.instructions.truncate(emitted.position);
            self.last_instruction = mem::replace(&mut self.previous_instruction, None);
        }
    }

    pub fn replace_last_pop_with_return(&mut self) {
        if let Some(last) = &self.last_instruction {
            let position = last.position;
            self.replace_instruction(position, code::make(OpCode::ReturnValue));
            self.last_instruction = Some(EmittedInstruction {
                position,
                op_code: OpCode::ReturnValue,
            });
        }
    }
}

#[derive(Debug)]
pub enum CompileError {
    UnknownOperator(Infix),
    UndefinedVariable(String),
    TooManyConstants,
    TooManyParams,
    TooManyLocals,
    TooManyFrees,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::UnknownOperator(infix) => write!(f, "unknown operator: {}", infix),
            CompileError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            CompileError::TooManyConstants => write!(f, "too many constants"),
            CompileError::TooManyParams => write!(f, "too many parameters for a function"),
            CompileError::TooManyLocals => write!(f, "too many local bindings in a function"),
            CompileError::TooManyFrees => write!(f, "too many free bindings in a function"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::ast::Program;
    use crate::code::{
        make, make_u16, make_u16_u8, make_u8, print_instructions, CompiledFunction, Constant,
        Instructions, OpCode,
    };
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn compile() {
        test_compile(vec![
            (
                "1 + 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Add),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 - 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Sub),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 * 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Mul),
                    make(OpCode::Pop),
                ],
            ),
            (
                "2 / 1",
                vec![Constant::Integer(2), Constant::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Div),
                    make(OpCode::Pop),
                ],
            ),
            ("true", vec![], vec![make(OpCode::True), make(OpCode::Pop)]),
            (
                "false",
                vec![],
                vec![make(OpCode::False), make(OpCode::Pop)],
            ),
            (
                "1 == 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Equal),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 != 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::NotEqual),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 > 2",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::GreaterThan),
                    make(OpCode::Pop),
                ],
            ),
            (
                "1 < 2",
                vec![Constant::Integer(2), Constant::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::GreaterThan),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn prefix_expression() {
        test_compile(vec![
            (
                "!true",
                vec![],
                vec![make(OpCode::True), make(OpCode::Bang), make(OpCode::Pop)],
            ),
            (
                "-123",
                vec![Constant::Integer(123)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make(OpCode::Minus),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn if_expression() {
        test_compile(vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Constant::Integer(10), Constant::Integer(3333)],
                vec![
                    make(OpCode::True),
                    make_u16(OpCode::JumpIfNotTruthy, 10),
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Jump, 11),
                    make(OpCode::Null),
                    make(OpCode::Pop),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Pop),
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Constant::Integer(10),
                    Constant::Integer(20),
                    Constant::Integer(3333),
                ],
                vec![
                    make(OpCode::True),
                    make_u16(OpCode::JumpIfNotTruthy, 10),
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Jump, 13),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Pop),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn global_let_statements() {
        test_compile(vec![
            (
                "let one = 1; let two = 2;",
                vec![Constant::Integer(1), Constant::Integer(2)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::SetGlobal, 1),
                ],
            ),
            (
                "let one = 1; one;",
                vec![Constant::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let one = 1; let two = one; two;",
                vec![Constant::Integer(1)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make_u16(OpCode::SetGlobal, 1),
                    make_u16(OpCode::GetGlobal, 1),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn string_expressions() {
        test_compile(vec![
            (
                r#""hello""#,
                vec![Constant::String("hello".to_string())],
                vec![make_u16(OpCode::Constant, 0), make(OpCode::Pop)],
            ),
            (
                r#""hel" + "lo""#,
                vec![
                    Constant::String("hel".to_string()),
                    Constant::String("lo".to_string()),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Add),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn array_expressions() {
        test_compile(vec![
            (
                "[]",
                vec![],
                vec![make_u16(OpCode::Array, 0), make(OpCode::Pop)],
            ),
            (
                "[1, 2, 3]",
                vec![
                    Constant::Integer(1),
                    Constant::Integer(2),
                    Constant::Integer(3),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make_u16(OpCode::Array, 3),
                    make(OpCode::Pop),
                ],
            ),
            (
                "[1 - 2, 3 + 4, 5 * 6]",
                vec![
                    Constant::Integer(1),
                    Constant::Integer(2),
                    Constant::Integer(3),
                    Constant::Integer(4),
                    Constant::Integer(5),
                    Constant::Integer(6),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make(OpCode::Sub),
                    make_u16(OpCode::Constant, 2),
                    make_u16(OpCode::Constant, 3),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Constant, 5),
                    make(OpCode::Mul),
                    make_u16(OpCode::Array, 3),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn hash_expression() {
        test_compile(vec![
            (
                "{}",
                vec![],
                vec![make_u16(OpCode::Hash, 0), make(OpCode::Pop)],
            ),
            (
                r#"{ 1: "hello", "foo": 1 + 2 }"#,
                vec![
                    Constant::String("foo".to_string()),
                    Constant::Integer(1),
                    Constant::Integer(2),
                    Constant::Integer(1),
                    Constant::String("hello".to_string()),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 3),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Hash, 2),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn index_expression() {
        test_compile(vec![
            (
                "[1, 2][0]",
                vec![
                    Constant::Integer(1),
                    Constant::Integer(2),
                    Constant::Integer(0),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Array, 2),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Index),
                    make(OpCode::Pop),
                ],
            ),
            (
                r#"{"foo": 1 + 2, "bar": 3 + 4}["bar"]"#,
                vec![
                    Constant::String("bar".to_string()),
                    Constant::Integer(3),
                    Constant::Integer(4),
                    Constant::String("foo".to_string()),
                    Constant::Integer(1),
                    Constant::Integer(2),
                    Constant::String("bar".to_string()),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make(OpCode::Add),
                    make_u16(OpCode::Constant, 3),
                    make_u16(OpCode::Constant, 4),
                    make_u16(OpCode::Constant, 5),
                    make(OpCode::Add),
                    make_u16(OpCode::Hash, 2),
                    make_u16(OpCode::Constant, 6),
                    make(OpCode::Index),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn functions() {
        test_compile(vec![
            (
                "fn() { return 5 + 10 }",
                vec![
                    Constant::Integer(5),
                    Constant::Integer(10),
                    compiled_function(
                        0,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 2, 0), make(OpCode::Pop)],
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Constant::Integer(5),
                    Constant::Integer(10),
                    compiled_function(
                        0,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 2, 0), make(OpCode::Pop)],
            ),
            (
                "fn() { 1; 2 }",
                vec![
                    Constant::Integer(1),
                    Constant::Integer(2),
                    compiled_function(
                        0,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make(OpCode::Pop),
                            make_u16(OpCode::Constant, 1),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 2, 0), make(OpCode::Pop)],
            ),
            (
                "fn() { }",
                vec![compiled_function(0, 0, vec![make(OpCode::Return)])],
                vec![make_u16_u8(OpCode::Closure, 0, 0), make(OpCode::Pop)],
            ),
        ]);
    }

    #[test]
    fn function_calls() {
        test_compile(vec![
            (
                "fn() { 24 }();",
                vec![
                    Constant::Integer(24),
                    compiled_function(
                        0,
                        0,
                        vec![make_u16(OpCode::Constant, 0), make(OpCode::ReturnValue)],
                    ),
                ],
                vec![
                    make_u16_u8(OpCode::Closure, 1, 0),
                    make_u8(OpCode::Call, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let noArg = fn() { 24 }; noArg();",
                vec![
                    Constant::Integer(24),
                    compiled_function(
                        0,
                        0,
                        vec![make_u16(OpCode::Constant, 0), make(OpCode::ReturnValue)],
                    ),
                ],
                vec![
                    make_u16_u8(OpCode::Closure, 1, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make_u8(OpCode::Call, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let oneArg = fn(a) { a }; oneArg(24);",
                vec![
                    compiled_function(
                        1,
                        1,
                        vec![make_u8(OpCode::GetLocal, 0), make(OpCode::ReturnValue)],
                    ),
                    Constant::Integer(24),
                ],
                vec![
                    make_u16_u8(OpCode::Closure, 0, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u8(OpCode::Call, 1),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
                vec![
                    compiled_function(
                        3,
                        3,
                        vec![
                            make_u8(OpCode::GetLocal, 0),
                            make(OpCode::Pop),
                            make_u8(OpCode::GetLocal, 1),
                            make(OpCode::Pop),
                            make_u8(OpCode::GetLocal, 2),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    Constant::Integer(24),
                    Constant::Integer(25),
                    Constant::Integer(26),
                ],
                vec![
                    make_u16_u8(OpCode::Closure, 0, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make_u16(OpCode::Constant, 1),
                    make_u16(OpCode::Constant, 2),
                    make_u16(OpCode::Constant, 3),
                    make_u8(OpCode::Call, 3),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    #[test]
    fn let_statement_scopes() {
        test_compile(vec![
            (
                "let num = 55; fn() { num }",
                vec![
                    Constant::Integer(55),
                    compiled_function(
                        0,
                        0,
                        vec![make_u16(OpCode::GetGlobal, 0), make(OpCode::ReturnValue)],
                    ),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16_u8(OpCode::Closure, 1, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "let num = 55; num",
                vec![Constant::Integer(55)],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16(OpCode::GetGlobal, 0),
                    make(OpCode::Pop),
                ],
            ),
            (
                "fn() {
                     let a = 55;
                     let b = 77;
                     a + b
                 }",
                vec![
                    Constant::Integer(55),
                    Constant::Integer(77),
                    compiled_function(
                        2,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 0),
                            make_u8(OpCode::SetLocal, 0),
                            make_u16(OpCode::Constant, 1),
                            make_u8(OpCode::SetLocal, 1),
                            make_u8(OpCode::GetLocal, 0),
                            make_u8(OpCode::GetLocal, 1),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 2, 0), make(OpCode::Pop)],
            ),
        ]);
    }

    #[test]
    fn builtin_functions() {
        test_compile(vec![
            (
                "len([]); push([], 1);",
                vec![Constant::Integer(1)],
                vec![
                    make_u8(OpCode::GetBuiltin, 0),
                    make_u16(OpCode::Array, 0),
                    make_u8(OpCode::Call, 1),
                    make(OpCode::Pop),
                    make_u8(OpCode::GetBuiltin, 4),
                    make_u16(OpCode::Array, 0),
                    make_u16(OpCode::Constant, 0),
                    make_u8(OpCode::Call, 2),
                    make(OpCode::Pop),
                ],
            ),
            (
                "fn() { len([], 1); }",
                vec![
                    Constant::Integer(1),
                    compiled_function(
                        0,
                        0,
                        vec![
                            make_u8(OpCode::GetBuiltin, 0),
                            make_u16(OpCode::Array, 0),
                            make_u16(OpCode::Constant, 0),
                            make_u8(OpCode::Call, 2),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 1, 0), make(OpCode::Pop)],
            ),
        ]);
    }

    #[test]
    fn closures() {
        test_compile(vec![
            (
                "fn(a) { fn (b) { a + b } }",
                vec![
                    compiled_function(
                        1,
                        1,
                        vec![
                            make_u8(OpCode::GetFree, 0),
                            make_u8(OpCode::GetLocal, 0),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    compiled_function(
                        1,
                        1,
                        vec![
                            make_u8(OpCode::GetLocal, 0),
                            make_u16_u8(OpCode::Closure, 0, 1),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 1, 0), make(OpCode::Pop)],
            ),
            (
                "fn(a) { fn(b) { fn(c) { a + b + c } } }",
                vec![
                    compiled_function(
                        1,
                        1,
                        vec![
                            make_u8(OpCode::GetFree, 0),
                            make_u8(OpCode::GetFree, 1),
                            make(OpCode::Add),
                            make_u8(OpCode::GetLocal, 0),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    compiled_function(
                        1,
                        1,
                        vec![
                            make_u8(OpCode::GetFree, 0),
                            make_u8(OpCode::GetLocal, 0),
                            make_u16_u8(OpCode::Closure, 0, 2),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    compiled_function(
                        1,
                        1,
                        vec![
                            make_u8(OpCode::GetLocal, 0),
                            make_u16_u8(OpCode::Closure, 1, 1),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![make_u16_u8(OpCode::Closure, 2, 0), make(OpCode::Pop)],
            ),
            (
                "let global = 55;
                 fn() {
                     let a = 66;
                     fn() {
                         let b = 77;
                         fn() {
                             let c = 88;
                             global + a + b + c
                         }
                     }
                 }",
                vec![
                    Constant::Integer(55),
                    Constant::Integer(66),
                    Constant::Integer(77),
                    Constant::Integer(88),
                    compiled_function(
                        1,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 3),
                            make_u8(OpCode::SetLocal, 0),
                            make_u16(OpCode::GetGlobal, 0),
                            make_u8(OpCode::GetFree, 0),
                            make(OpCode::Add),
                            make_u8(OpCode::GetFree, 1),
                            make(OpCode::Add),
                            make_u8(OpCode::GetLocal, 0),
                            make(OpCode::Add),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    compiled_function(
                        1,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 2),
                            make_u8(OpCode::SetLocal, 0),
                            make_u8(OpCode::GetFree, 0),
                            make_u8(OpCode::GetLocal, 0),
                            make_u16_u8(OpCode::Closure, 4, 2),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                    compiled_function(
                        1,
                        0,
                        vec![
                            make_u16(OpCode::Constant, 1),
                            make_u8(OpCode::SetLocal, 0),
                            make_u8(OpCode::GetLocal, 0),
                            make_u16_u8(OpCode::Closure, 5, 1),
                            make(OpCode::ReturnValue),
                        ],
                    ),
                ],
                vec![
                    make_u16(OpCode::Constant, 0),
                    make_u16(OpCode::SetGlobal, 0),
                    make_u16_u8(OpCode::Closure, 6, 0),
                    make(OpCode::Pop),
                ],
            ),
        ]);
    }

    fn test_compile(tests: Vec<(&str, Vec<Constant>, Vec<Instructions>)>) {
        for (input, expected_constants, expected_instructions) in tests {
            let program = parse(input);

            let compiler = Compiler::new();
            let bytecode = match compiler.compile(&program) {
                Ok(bytecode) => bytecode,
                Err(error) => panic!("failed to compile input `{}`: {}", input, error),
            };

            // Compare instructions.
            assert_eq!(
                print_instructions(&bytecode.instructions),
                print_instructions(&expected_instructions.concat()),
                "\nfor `{}`",
                input
            );

            if bytecode.constants.len() != expected_constants.len() {
                assert_eq!(bytecode.constants, expected_constants, "\nfor {}", input);
            }

            // Pretty-print instructions in error messages
            let pairs = bytecode.constants.iter().zip(expected_constants.iter());
            for (i, (constant, expected_constant)) in pairs.enumerate() {
                match (constant, expected_constant) {
                    (Constant::CompiledFunction(actual), Constant::CompiledFunction(expected)) => {
                        assert_eq!(
                            actual.to_string(),
                            expected.to_string(),
                            "\nconstant (index {}) for {}",
                            i,
                            input
                        );
                    }
                    _ => {
                        assert_eq!(
                            constant, expected_constant,
                            "\nconstant (index {}) for {}",
                            i, input
                        );
                    }
                }
            }
        }
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        program
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() > 0 {
            panic!(
                "for input '{}', got parser errors: {:?}",
                parser.input(),
                errors
            );
        }
    }

    fn compiled_function(
        num_locals: u8,
        num_parameters: u8,
        nested_ins: Vec<Instructions>,
    ) -> Constant {
        Constant::CompiledFunction(CompiledFunction {
            instructions: nested_ins.concat(),
            num_locals,
            num_parameters,
        })
    }
}
