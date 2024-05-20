use std::{iter::Peekable, str::Chars, vec::IntoIter};

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    TokenEOF,
    TokenDef,
    TokenExtern,
    TokenIdentifier(String),
    TokenNumber(f64),
    TokenUnknown(char)
}

impl Token {
    pub fn get_char(&self) -> Option<char> {
        if let Token::TokenUnknown(ch) = &self {
            Some(*ch)
        } else {
            None
        }
    }
}

pub fn get_token<'a>(file : &mut Peekable<Chars<'a>>) -> Token {
    let mut first_char: char = '\0';
    while let Some(c) = file.next() {
        if !c.is_whitespace() {
            first_char = c;
            break;
        }
    }
    if first_char.is_alphabetic() {
        let mut identifier = String::new();
        identifier.push(first_char);
        while let Some(c) = file.peek() {
            if c.is_alphanumeric() {
                identifier.push(*c);
                file.next();
            } else {
                break;
            }
        }
        if identifier == "def" {
            return Token::TokenDef;
        }
        if identifier == "extern" {
            return Token::TokenExtern;
        }
        return Token::TokenIdentifier(identifier);
    }
    if first_char.is_ascii_digit() || first_char == '.' {
        let mut number_string = String::new();
        number_string.push(first_char);
        while let Some(c) = file.next() {
            if c.is_ascii_digit() || c == '.' {
                number_string.push(c);
            } else {
                break;
            }
        }
        return Token::TokenNumber(number_string.parse::<f64>().unwrap());
    }
    if first_char == '#' {
        let mut last_char: char = '\0';
        while let Some(c) = file.next() {
            if c == '\0' || c == '\n' || c == '\r' {
                last_char = c;
                break;
            }
        }
        if last_char != '\0' {
            return get_token(file);
        } else {
            return Token::TokenEOF;
        }
    }
    if first_char == '\0' {
        return Token::TokenEOF;
    }
    Token::TokenUnknown(first_char)
}

#[derive(Clone)]
pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new<'a>(mut file: Peekable<Chars<'a>>) -> Self {
        let mut tokens = vec![];
        loop {
            match get_token(&mut file) {
                Token::TokenEOF => { tokens.push(Token::TokenEOF); break; },
                token => tokens.push(token),
            }
        }
        Lexer {
            tokens,
        }
    }
    
}

impl IntoIterator for Lexer {
    type Item = Token;
    type IntoIter = IntoIter<Token>;
    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_token() {
        let test = " +   bit bit64 64 #test\ndef extern ";
        let mut chars = test.chars().peekable();
        assert_eq!(get_token(&mut chars), Token::TokenUnknown('+'));
        assert_eq!(get_token(&mut chars), Token::TokenIdentifier("bit".to_string()));
        assert_eq!(get_token(&mut chars), Token::TokenIdentifier("bit64".to_string()));
        assert_eq!(get_token(&mut chars), Token::TokenNumber(64.));
        assert_eq!(get_token(&mut chars), Token::TokenDef);
        assert_eq!(get_token(&mut chars), Token::TokenExtern);
        assert_eq!(get_token(&mut chars), Token::TokenEOF);
    }

    #[test]
    fn test_lexer_new() {
        let test = "fn tester(test) is 5 + 7.5 #test";
        let lexer = Lexer::new(test.chars().peekable());

        assert_eq!(lexer.tokens[0], Token::TokenIdentifier("fn".to_string()));
        assert_eq!(lexer.tokens[1], Token::TokenIdentifier("tester".to_string()));
        assert_eq!(lexer.tokens[2], Token::TokenUnknown('('));
        assert_eq!(lexer.tokens[3], Token::TokenIdentifier("test".to_string()));
        assert_eq!(lexer.tokens[4], Token::TokenUnknown(')'));
        assert_eq!(lexer.tokens[5], Token::TokenIdentifier("is".to_string()));
        assert_eq!(lexer.tokens[6], Token::TokenNumber(5.));
        assert_eq!(lexer.tokens[7], Token::TokenUnknown('+'));
        assert_eq!(lexer.tokens[8], Token::TokenNumber(7.5));
        assert_eq!(lexer.tokens[9], Token::TokenEOF);
    }
}
