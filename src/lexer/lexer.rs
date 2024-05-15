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
    file: &'a [u8],
    current_index: usize,
    tokens: Vec<Token>,
}

impl <'a> Lexer <'a> {
    pub fn new(file: &'a [u8]) -> Self {
        Lexer {
            file,
            current_index: 0,
            tokens: vec![],
        }
    }
    
    pub fn get_token(&'a mut self) -> Token {
        while self.file[self.current_index].is_ascii_whitespace() {
            self.current_index += 1;
        }
        if self.file[self.current_index].is_ascii_alphabetic() {
            let mut identifier = Vec::new();
            identifier.push(self.file[self.current_index]);
            while self.file[self.current_index].is_ascii_alphanumeric() {
                identifier.push(self.file[self.current_index]);
            }
            let identifier = String::from_utf8(identifier).unwrap();
            if identifier == "def" {
                return Token::TokenDef;
            }
            if identifier == "extern" {
                return Token::TokenExtern;
            }
            return Token::TokenIdentifier(identifier);
        }
        if self.file[self.current_index].is_ascii_digit() || self.file[self.current_index] == b'.' {
            let mut number_string = Vec::new();
            number_string.push(self.file[self.current_index]);
            self.current_index += 1;
            while self.file[self.current_index].is_ascii_digit() || self.file[self.current_index] == b'.' {
                number_string.push(self.file[self.current_index]);
                self.current_index += 1;
            }
            return Token::TokenNumber(String::from_utf8(number_string).unwrap().parse::<f64>().unwrap());
        }
        if self.file[self.current_index] == b'#' {
            self.current_index += 1;
            while self.file[self.current_index] != b'\0'
                && self.file[self.current_index] != b'\n'
                && self.file[self.current_index] != b'\r' {
                self.current_index += 1;
            }
            if self.file[self.current_index] != b'\0' {
                return self.get_token();
            }
        }
        if self.file[self.current_index] == b'\0' {
            return Token::TokenEOF;
        }
        let this_char: char = char::from(self.file[self.current_index]);
        self.current_index += 1;
        Token::TokenUnknown(this_char)
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
/*        assert_eq!(lexer.get_token(), Token::TokenIdentifier("bit64".to_string()));
        assert_eq!(lexer.get_token(), Token::TokenNumber(64.));
        assert_eq!(lexer.get_token(), Token::TokenDef);
        assert_eq!(lexer.get_token(), Token::TokenExtern);
        assert_eq!(lexer.get_token(), Token::TokenEOF);
*/
    }
}
