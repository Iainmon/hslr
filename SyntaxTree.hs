module SyntaxTree where

data AST = Unary Unary AST | Binary Binary AST AST | Ass_ String AST | Id_ String | Imparitive [AST] AST deriving (Eq, Ord)
data Unary = Cos_ | Sin_ | Length_ | Abs_ | Neg_ deriving (Eq, Ord)
data Binary = Mul_ | Add_ | Sub_ | Div_ deriving (Eq, Ord)

instance Show AST where
    show (Id_ str) = str
    show (Ass_ a b) = a ++ " = " ++ show b
    show (Imparitive statements a) = "(" ++ (foldl (++) "" $ map (\x -> show x ++ "\n") statements) ++ "\n" ++ show a ++ ")"
    show (Unary unary a) = show unary ++ "(" ++ show a ++ ")"
    show (Binary binary a b) = "(" ++ show a ++ " " ++ show binary ++ " " ++ show b ++ ")"

instance Show Unary where
    show Cos_ = "cos"
    show Sin_ = "sin"
    show Length_ = "length"
    show Abs_ = "abs"
    show Neg_ = "-"

instance Show Binary where
    show Mul_ = "*"
    show Add_ = "+"
    show Div_ = "/"
    show Sub_ = "-"

data SyntaxTree = Cos SyntaxTree | Sin SyntaxTree | Length SyntaxTree | Abs SyntaxTree | Neg SyntaxTree | Mul SyntaxTree SyntaxTree | Add SyntaxTree SyntaxTree | Sub SyntaxTree SyntaxTree | Div SyntaxTree SyntaxTree | Ass String SyntaxTree | Id String

glslAST :: AST -> String
glslAST (Id_ a) = a
glslAST (Unary cons a) = show cons ++ "(" ++ show a ++ ")"
-- glslAST (Unary Cos_ a) = "cos(" ++ glslAST a ++ ")"
-- glslAST (Unary Sin_ a) = "sin(" ++ glslAST a ++ ")"
-- glslAST (Unary Length_ a) = "length(" ++ glslAST a ++ ")"
-- glslAST (Unary Abs_ a) = "abs(" ++ glslAST a ++ ")"
glslAST (Binary Add_ a b) = "(" ++ glslAST a ++ " + " ++ glslAST b ++ ")"
glslAST (Binary Mul_ a b) = "(" ++ glslAST a ++ " * " ++ glslAST b ++ ")"
glslAST (Binary Sub_ a b) = "(" ++ glslAST a ++ " - " ++ glslAST b ++ ")"
glslAST (Binary Div_ a b) = "(" ++ glslAST a ++ " / " ++ glslAST b ++ ")"
glslAST (Unary Neg_ a) = "(-" ++ glslAST a ++ ")"
glslAST (Ass_ a b) = a ++ " = " ++ glslAST b ++ ";\n"
