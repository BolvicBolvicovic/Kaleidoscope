struct NumberExprAST(f64);
struct VariableExprAST(String);
struct BinaryExprAST(char);

enum ExprAST {
    NumberExprAST(f64),
    VariableExprAST(String),
    BinaryExprAST(char, Box<ExprAST>, Box<ExprAST>),
    //TODO: CallExprAST
}
