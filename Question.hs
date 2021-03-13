

data Type = Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq)
data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq)
data AST = Call String CallType | Id String deriving (Eq)
data SyntaxTree = 
     Node Type AST
    | Imparitive [SyntaxTree] SyntaxTree
    | Assignment Type String SyntaxTree deriving (Eq)

class (Show t) => GLiteral t where
    represent :: t -> String
    syntaxNode :: t -> SyntaxTree
