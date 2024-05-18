use std::str::Chars;

#[derive(PartialEq, Debug)]
pub enum Token {
    TokenEOF,
    TokenDef,
    TokenExtern,
    TokenIdentifier(String),
    TokenNumber(f64),
    TokenUnknown(char)
}

pub struct Lexer <'a> {
    file: &'a Chars,
    tokens: Vec<Token>,
}

impl <'a> Lexer <'a> {
    pub fn new(file: &'a Chars) -> Self {
        Lexer {
            file,
            tokens: vec![],
        }
    }
    
    pub fn get_token(& mut self) -> Token {
        let first_char: char = '\0';
        while let Some(c) = &mut self.file.next() {
            if !c.is_whitespace() {
                first_char = *c;
                break;
            }
        }
        if first_char.is_alphabetic() {
            let mut identifier = String::new();
            identifier.push(first_char);
            while let Some(c) = &mut self.file.next() {
                if c.is_alphabetic() {
                    identifier.push(*c);
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
            while let Some(c) = &mut self.file.next() {
                if c.is_ascii_digit() || *c == '.' {
                    number_string.push(*c);
                } else {
                    break;
                }
            }
            return Token::TokenNumber(number_string.parse::<f64>().unwrap());
        }
        if first_char == '#' {
            while let Some(c) = &mut self.file.next() {
                if *c == '\0' && *c == '\n' && *c == '\r' {
                    break;
                }
            }
            if !&self.file.eq(None) {
                return self.get_token();
            }
        }
        if first_char == '\0' {
            return Token::TokenEOF;
        }
        Token::TokenUnknown(first_char)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_token() {
        let test = "bit bit64 64 #test\ndef extern\0".as_bytes();
        let mut lexer = Lexer::new(test);

        assert_eq!(lexer.get_token(), Token::TokenIdentifier("bit".to_string()));
        assert_eq!(lexer.get_token(), Token::TokenIdentifier("bit64".to_string()));
        assert_eq!(lexer.get_token(), Token::TokenNumber(64.));
        assert_eq!(lexer.get_token(), Token::TokenDef);
        assert_eq!(lexer.get_token(), Token::TokenExtern);
        assert_eq!(lexer.get_token(), Token::TokenEOF);
    }
}
