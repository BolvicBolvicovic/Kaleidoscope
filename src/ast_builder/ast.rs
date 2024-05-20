use std::{iter::Peekable, str::Chars, vec::IntoIter};
use super::{Lexer, Token};

#[derive(PartialEq, Debug, Clone)]
enum ExprAST {
    NumberExprAST(f64),                                 // Value
    VariableExprAST(String),                            // Name
    BinaryExprAST(char, Box<ExprAST>, Box<ExprAST>),    // Op, LHS, RHS
    CallExprAST(String, Vec<ExprAST>),                  // Callee, Args 
    PrototypeAST(String, Vec<String>),                  // Name, Args
    FunctionAST(Box<ExprAST>, Box<ExprAST>),            // Proto, Expr
    EOF,
}

struct Parser {
    lexer: Lexer,
    parsed_file: Vec<ExprAST>,
}

impl Parser {
    pub fn new<'a>(file: Peekable<Chars<'a>>) -> Self {
        let lexer = Lexer::new(file);
        let parsed_file = vec![];
        Parser {
            lexer,
            parsed_file,
        }
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let mut lexer = self.lexer.clone().into_iter();
        while let Some(token) = lexer.next() {
            self.parsed_file.push(parse_expression(token, &mut lexer)?);
        }
        Ok(())
    }
}

fn parse_expression(token: Token, iter: &mut IntoIter<Token>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    let lhs = parse_primary(token, iter)?;
    parse_bin_op_rhs(0, lhs)
}

fn parse_primary(token: Token, iter: &mut IntoIter<Token>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    match token {
        Token::TokenIdentifier(_)   => parse_identifier_expr(token, iter),
        Token::TokenNumber(_)       => parse_number_expr(token),
        Token::TokenUnknown('(')    => parse_parenthesis_expr(iter),
        Token::TokenEOF             => Ok(ExprAST::EOF),
        token                       => Err(format!("Token {:?} when expected an expression", token).into()),
    }
}

fn parse_bin_op_rhs(expr_prec: i32, lhs: ExprAST) -> Result<ExprAST, Box<dyn std::error::Error>> {
    loop {
        let tok_prec = -1; // get_tok_precedence()
        if tok_prec < expr_prec {
            return Ok(lhs);
        }
    }
}

fn parse_number_expr(num: Token) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Token::TokenNumber(num) = num {
        Ok(ExprAST::NumberExprAST(num))
    } else {
        Err("Error parsing number expr".into())
    }
}

fn parse_parenthesis_expr(iter: &mut IntoIter<Token>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Some(token) = iter.next() {
        let parsed_expr = parse_expression(token, iter);
        if let Some(ch) = iter.next() && ch != Token::TokenUnknown(')') {
            Err("Error: Expected ')'".into())
        } else {
            parsed_expr
        }
    } else {
        Err("Error parsing parenthesis expr".into())
    }
}

fn parse_identifier_expr(token: Token, iter: &mut IntoIter<Token>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    let id_name;
    if let Token::TokenIdentifier(id) = token {
        id_name = id;
    } else {
        return Err("Error usage parse_identifier_expr".into());
    }
    
    let mut args = vec![];
    if let Some(token) = iter.next() {
        println!("{:?}",token);
        if let Token::TokenUnknown(ch) = token {
            if ch != '(' {
                return Ok(ExprAST::VariableExprAST(id_name));
            } else {
                while let Some(token) = iter.next() {
                    if let Token::TokenUnknown(ch) = token && ch == ')' {
                        break;
                    }
                    let arg = parse_expression(token, iter)?;
                    args.push(arg);
                    if let Some(token) = iter.next() && let Token::TokenUnknown(ch) = token {
                        if ch == ')' {
                            break;
                        } else if ch != ',' {
                            return Err("Expected ')' or ',' in argument list".into());
                        }

                    }
                }
            }
        } else {
            return Ok(ExprAST::VariableExprAST(id_name));
        }
    } else {
        return Ok(ExprAST::VariableExprAST(id_name));
    }
    Ok(ExprAST::CallExprAST(id_name, args))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier_expr() -> Result<(), Box<dyn std::error::Error>> {
        let test = "test";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;
        assert_eq!(parser.parsed_file[0], ExprAST::VariableExprAST("test".to_string()));

        let test = "test(iden)";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;
        assert_eq!(parser.parsed_file[0], ExprAST::CallExprAST("test".to_string(), [ExprAST::VariableExprAST("iden".to_string())].to_vec()));
        Ok(())
    }
}
