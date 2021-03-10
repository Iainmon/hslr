
data Type = Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq)

data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq)
data AST = Call String CallType
    | Id String deriving (Eq)
data SyntaxTree = 
     Node Type AST
    | Imparitive [SyntaxTree] SyntaxTree
    | Assignment Type String SyntaxTree deriving (Eq)


instance Show Type where
    show Int = "int"
    show Float = "float"
    show Bool = "bool"
    show Vector2 = "vec2"
    show Vector3 = "vec3"
    show Vector4 = "vec4"

parenthisize :: [String] -> String
parenthisize [] = ""
parenthisize (a:[]) = a ++ ")"
parenthisize (a:as) = "(" ++ a ++ "," ++ parenthisize as

instance Show SyntaxTree where
    show (Node _ ast) = show ast
    show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldl (++) "" $ map (++";\n") $ map show assignments
    show (Assignment ty name a) = show ty ++ " " ++ name ++ " = " ++ show a

instance Show CallType where
    show (Unary (a)) = "(" ++ (parenthisize $ map show [a])
    show (Binary (a,b)) = parenthisize $ map show [a,b]
    show (Trinary (a,b,c)) = parenthisize $ map show [a,b,c]

instance Show AST where
    show (Id a) = a
    -- show (Assignment t name a) = show t ++ " " ++ name ++ " = " ++ show a
    -- show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldl (++) "" $ map (++";\n") $ map show assignments
    -- show (Call name (Unary (a)))       = name ++ (parenthisize $ map show [a])
    -- show (Call name (Binary (a,b)))    = name ++ (parenthisize $ map show [a,b])
    -- show (Call name (Trinary (a,b,c))) = name ++ (parenthisize $ map show [a,b,c])
    show (Call name call) = name ++ show call

cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine _ = undefined

ast = Node Float (Id "iain")