use std::{collections::HashMap, marker::Sized};

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

enum InputElement {
    Value(Value),
    Operation(Operation),
    Method(Method),
    Definition(String),
    Invalid,
}

trait Action {
    fn from_str(value: &str) -> Option<Self>
    where
        Self: Sized;
}

enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Action for Operation {
    fn from_str(sign: &str) -> Option<Self> {
        match sign {
            "+" => Some(Operation::Add),
            "-" => Some(Operation::Subtract),
            "*" => Some(Operation::Multiply),
            "/" => Some(Operation::Divide),
            _ => None,
        }
    }
}

enum Method {
    Drop,
    Dup,
    Over,
    Swap,
}

impl Action for Method {
    fn from_str(action: &str) -> Option<Self> {
        match action {
            "drop" => Some(Method::Drop),
            "dup" => Some(Method::Dup),
            "over" => Some(Method::Over),
            "swap" => Some(Method::Swap),
            _ => None,
        }
    }
}

pub struct Forth {
    stack: Vec<Value>,
    definitions: HashMap<String, Vec<String>>,
}

impl Forth {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            definitions: HashMap::new(),
        }
    }

    pub fn stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let input = self.parse_input(input);
        for item in input {
            match item {
                InputElement::Value(number) => self.push_to_stack(number),
                InputElement::Operation(operation) => self.operate(operation),
                InputElement::Method(action) => self.run_method(action),
                InputElement::Definition(definition) => self.define(definition),
                _ => Err(Error::UnknownWord),
            }?
        }
        Ok(())
    }

    fn parse_input(&mut self, input: &str) -> Vec<InputElement> {
        if input.starts_with(":") {
            vec![InputElement::Definition(input.to_lowercase())]
        } else {
            input
                .to_lowercase()
                .split(" ")
                .flat_map(|part| match self.definitions.get(part) {
                    Some(parts) => parts.clone(),
                    _ => vec![part.to_string()],
                })
                .map(|item| {
                    if let Ok(num) = item.parse::<Value>() {
                        InputElement::Value(num)
                    } else if let Some(operation) = Operation::from_str(&item) {
                        InputElement::Operation(operation)
                    } else if let Some(method) = Method::from_str(&item) {
                        InputElement::Method(method)
                    } else {
                        InputElement::Invalid
                    }
                })
                .collect()
        }
    }

    fn push_to_stack(&mut self, number: Value) -> ForthResult {
        self.stack.push(number);
        Ok(())
    }

    fn operate(&mut self, operation: Operation) -> ForthResult {
        if self.stack.len() < 2 {
            return Err(Error::StackUnderflow);
        }
        let first = self.stack.pop().unwrap();
        let second = self.stack.pop().unwrap();
        let new_value = match operation {
            Operation::Add => first + second,
            Operation::Subtract => second - first,
            Operation::Multiply => first * second,
            Operation::Divide => {
                if first == 0 {
                    return Err(Error::DivisionByZero);
                }
                second / first
            }
        };
        self.stack.push(new_value);
        Ok(())
    }

    fn run_method(&mut self, method: Method) -> ForthResult {
        let stack_len = self.stack.len();
        let min_len = match method {
            Method::Swap | Method::Over => 2,
            _ => 1,
        };
        if stack_len < min_len {
            return Err(Error::StackUnderflow);
        }
        match method {
            Method::Dup | Method::Over => {
                let index = match method {
                    Method::Over => stack_len - 2,
                    _ => stack_len - 1,
                };
                let last_value = self.stack.iter().nth(index).unwrap().clone();
                self.stack.push(last_value);
            }
            Method::Drop => {
                self.stack.pop();
            }
            Method::Swap => {
                let first = self.stack.pop().unwrap();
                let second = self.stack.pop().unwrap();
                self.stack.append(&mut [first, second].to_vec());
            }
        }
        Ok(())
    }

    fn define(&mut self, input: String) -> ForthResult {
        if !input.ends_with(";") {
            return Err(Error::InvalidWord);
        }
        let input: Vec<String> = input[2..input.len() - 2]
            .split(" ")
            .map(|c| c.to_string())
            .collect();
        let command = &input.first().unwrap();
        if let Ok(_) = command.parse::<Value>() {
            return Err(Error::InvalidWord);
        }
        let args: Vec<String> = input.iter().skip(1).map(|c| c.to_string()).collect();
        self.definitions.insert(command.to_string(), args);
        Ok(())
    }
}
