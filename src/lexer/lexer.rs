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

pub fn get_token<'a>(file : &mut Chars<'a>) -> Token {
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
        while let Some(c) = file.next() {
            if c.is_alphanumeric() {
                identifier.push(c);
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
            if c == '\0' && c == '\n' && c == '\r' {
                last_char = c;
                break;
            }
        }
        if last_char != '\0' {
            get_token(file);
        }
    }
    if first_char == '\0' {
        return Token::TokenEOF;
    }
    Token::TokenUnknown(first_char)
}

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new<'a>(mut file: Chars<'a>) -> Self {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_token() {
        let test = "+   bit bit64 64 #test\ndef extern ";
        let mut chars = test.chars();
        assert_eq!(get_token(&mut chars), Token::TokenUnknown('+'));
        assert_eq!(get_token(&mut chars), Token::TokenIdentifier("bit".to_string()));
        assert_eq!(get_token(&mut chars), Token::TokenIdentifier("bit64".to_string()));
        assert_eq!(get_token(&mut chars), Token::TokenNumber(64.));
        assert_eq!(get_token(&mut chars), Token::TokenDef);
        assert_eq!(get_token(&mut chars), Token::TokenExtern);
        assert_eq!(get_token(&mut chars), Token::TokenEOF);
    }
}
