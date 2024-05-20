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
        let mut lexer = self.lexer.clone().into_iter().peekable();
        while let Some(token) = lexer.next() {
            match token {
                Token::TokenEOF => break,
                Token::TokenUnknown(';') => continue,
                Token::TokenDef => self.parsed_file.push(parse_definition(&mut lexer)?),
                Token::TokenExtern => self.parsed_file.push(parse_extern(&mut lexer)?),
                _ => self.parsed_file.push(parse_top_level_expr(&mut lexer)?)
            }
        }
        Ok(())
    }
}

fn parse_expression(token: Token, iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    let lhs = parse_primary(token, iter)?;
    parse_bin_op_rhs(iter, 0, lhs)
}

fn parse_primary(token: Token, iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    match token {
        Token::TokenIdentifier(_)   => parse_identifier_expr(token, iter),
        Token::TokenNumber(_)       => parse_number_expr(token),
        Token::TokenUnknown('(')    => parse_parenthesis_expr(iter),
        Token::TokenEOF             => Ok(ExprAST::EOF),
        token                       => Err(format!("Token {:?} when expected an expression", token).into()),
    }
}

fn parse_bin_op_rhs(iter: &mut Peekable<IntoIter<Token>>, expr_prec: i8, mut lhs: ExprAST) -> Result<ExprAST, Box<dyn std::error::Error>> {
    loop {
        let tok_prec = get_tok_precedence(iter);
        if tok_prec < expr_prec {
            return Ok(lhs);
        }
        let bin_op = iter.next().unwrap().get_char().unwrap();
        let mut rhs;
        if let Some(token) = iter.next() {
            rhs = parse_primary(token, iter)?;
        } else {
            return Err("right element missing in binary operation".into());
        }
        let next_prec = get_tok_precedence(iter);
        if tok_prec < next_prec {
            rhs = parse_bin_op_rhs(iter, tok_prec + 1, rhs)?;
        }
        lhs = ExprAST::BinaryExprAST(bin_op, Box::new(lhs), Box::new(rhs));
    }
}

fn get_tok_precedence(iter: &mut Peekable<IntoIter<Token>>) -> i8 {
    if let Some(Token::TokenUnknown(ch)) = iter.peek() {
        match *ch {
            '<' => 10,
            '+' => 20,
            '-' => 20,
            '*' => 40,
            '/' => 40,
            _   => -1,
        }
    } else {
        -1
    }
}

fn parse_number_expr(num: Token) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Token::TokenNumber(num) = num {
        Ok(ExprAST::NumberExprAST(num))
    } else {
        Err("Error parsing number expr".into())
    }
}

fn parse_parenthesis_expr(iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
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

fn parse_identifier_expr(token: Token, iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    let id_name;
    if let Token::TokenIdentifier(id) = token {
        id_name = id;
    } else {
        return Err("Error usage parse_identifier_expr".into());
    }
    
    let mut args = vec![];
    if let Some(Token::TokenUnknown(ch)) = iter.peek() {
        if *ch != '(' {
            return Ok(ExprAST::VariableExprAST(id_name));
        } else {
            iter.next();
            while let Some(token) = iter.next() {
                if let Token::TokenUnknown(')') = token {
                    break;
                }
                let arg = parse_expression(token, iter)?;
                args.push(arg);
                if let Some(Token::TokenUnknown(ch)) = iter.next() {
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
    Ok(ExprAST::CallExprAST(id_name, args))
}

fn parse_prototype(token: Token, iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Token::TokenIdentifier(ident) = token {
        let fn_name = ident;
        if let Some(Token::TokenUnknown('(')) = iter.next() {
            let mut args = vec![];
            while let Some(Token::TokenIdentifier(arg)) = iter.peek() {
                args.push(arg.to_owned());
                iter.next();
            }
            if let Some(Token::TokenUnknown(')')) = iter.next() {
                Ok(ExprAST::PrototypeAST(fn_name, args))
            } else {
                Err("Expected ')' in prototype".into())
            }
        } else {
            Err("Expected '(' in prototype".into())
        }
    } else {
        Err("Expected function name in prototype".into())
    }
}

fn parse_definition(iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    let fn_name;
    if let Some(token) = iter.next() {
        fn_name = token;
    } else {
        return Err("error parse_definition".into());
    }
    let proto = parse_prototype(fn_name, iter)?;
    let expr_tok;
    if let Some(token) = iter.next() {
        expr_tok = token;
    } else {
        return Err("error missing expr in parse_definition".into());
    }
    let expr = parse_expression(expr_tok, iter)?;
    Ok(ExprAST::FunctionAST(Box::new(proto), Box::new(expr)))
}

fn parse_extern(iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Some(token) = iter.next() {
        parse_prototype(token, iter)
    } else {
        Err("error parse_extern".into())
    }
}

fn parse_top_level_expr(iter: &mut Peekable<IntoIter<Token>>) -> Result<ExprAST, Box<dyn std::error::Error>> {
    if let Some(token) = iter.next() {
        if let Ok(e) = parse_expression(token, iter) {
            let proto = ExprAST::PrototypeAST(String::new(), vec![]);
            Ok(ExprAST::FunctionAST(Box::new(proto), Box::new(e)))
        } else {
            Err("error in parse_top_level_expr".into())
        }
    } else {
        Err("error EOF parse_top_level_expr".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier_expr() -> Result<(), Box<dyn std::error::Error>> {
        let mut test = vec![Token::TokenIdentifier("test".to_string())].into_iter().peekable();
        assert_eq!(parse_identifier_expr(test.next().unwrap(), &mut test)?, ExprAST::VariableExprAST("test".to_string()));

        let test = "test(iden)";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;
        assert_eq!(parser.parsed_file[0], ExprAST::CallExprAST("test".to_string(), [ExprAST::VariableExprAST("iden".to_string())].to_vec()));
        Ok(())
    }

    #[test]
    fn test_parse_bin_op() -> Result<(), Box<dyn std::error::Error>> {
        let test = "a+b";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;
        assert_eq!(parser.parsed_file[0], ExprAST::BinaryExprAST(
                '+',
                Box::new(ExprAST::VariableExprAST("a".to_string())),
                Box::new(ExprAST::VariableExprAST("b".to_string()))
        ));

        let test = "a+b*c";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;
        assert_eq!(parser.parsed_file[0], ExprAST::BinaryExprAST(
                '+',
                Box::new(ExprAST::VariableExprAST("a".to_string())),
                Box::new(ExprAST::BinaryExprAST(
                    '*',
                    Box::new(ExprAST::VariableExprAST("b".to_string())),
                    Box::new(ExprAST::VariableExprAST("c".to_string()))
                ),
        )));
        Ok(())
    }

    #[test]
    fn test_definition() -> Result<(), Box<dyn std::error::Error>> {
       let test = "def foo(x y) x+foo(y, 4.0)"; 
       let mut parser = Parser::new(test.chars().peekable());
       parser.parse()?;
       assert_eq!(parser.parsed_file[0], ExprAST::FunctionAST(
            Box::new(ExprAST::PrototypeAST(
                "foo".to_string(),
                Vec::from([
                    "x".to_string(),
                    "y".to_string(),
                ])
            )),
            Box::new(ExprAST::BinaryExprAST(
                '+',
                Box::new(ExprAST::VariableExprAST("x".to_string())),
                Box::new(ExprAST::CallExprAST(
                    "foo".to_string(), 
                    Vec::from([
                        ExprAST::VariableExprAST("y".to_string()),
                        ExprAST::NumberExprAST(4.),
                ])))
            ))
        ));
       Ok(())
    }

    #[test]
    fn test_extern() -> Result<(), Box<dyn std::error::Error>> {
        let test = "extern sin(a)";
        let mut parser = Parser::new(test.chars().peekable());
        parser.parse()?;

        assert_eq!(parser.parsed_file[0], ExprAST::PrototypeAST(
            "sin".to_string(),
            Vec::from([
                "a".to_string()
            ]),
        ));
        Ok(())
    }
}
