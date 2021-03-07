import Data.Map as Map



data AST = Unary Unary AST | Binary Binary AST AST | Ass_ String AST | Id_ String deriving (Eq, Ord)
data Unary = Cos_ | Sin_ | Length_ | Abs_ | Neg_ deriving (Eq, Ord)
data Binary = Mul_ | Add_ | Sub_ | Div_ deriving (Eq, Ord)

instance Show AST where
    show (Id_ str) = str
    show (Ass_ a b) = a ++ " = " ++ show b
    show (Unary unary a) = show unary ++ show a
    show (Binary binary a b) = show a ++ show binary ++ show b

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

ast :: SyntaxTree
ast = Add (Mul (Cos (Id "uv")) (Sin (Id "uv"))) (Id "1") -- cos(uv) * sin(uv) 

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

data Color = Color { red :: Double
                   , green :: Double
                   , blue :: Double
                   } deriving (Eq)

data Location = UV Double Double

glslColor c = "vec3(" ++ (show (red c)) ++ ", " ++ (show (green c)) ++ ", " ++ (show (blue c)) ++ ")"

data Bias = BP String Double Double -- name of function, b, s
data Layer = Base Color | Combination Bias Layer Layer

glslLayer (Base c) = glslColor c
glslLayer (Combination (BP f b s) l1 l2) = "mix(" ++ glslLayer l1 ++ "," ++ glslLayer l2 ++ "," ++ "smoothstep(" ++ show b ++ ", " ++ (show (b+s)) ++ ", " ++ f ++ "(uv)))"

redC = Color { red = 1.0, green = 0.8, blue = 0.2 }
blueC = Color { red = 1.0, green = 0.8, blue = 0.2 }

layer = Combination (BP "f" 0.5 0.001) (Base redC) (Base blueC)


convertToBackendSyntaxTree :: SyntaxTree -> AST
convertToBackendSyntaxTree (Id a) = Id_ a
convertToBackendSyntaxTree (Ass a b) = Ass_ a (convertToBackendSyntaxTree b)
convertToBackendSyntaxTree (Sin a) = Unary Sin_ (convertToBackendSyntaxTree a)
convertToBackendSyntaxTree (Cos a) = Unary Cos_ (convertToBackendSyntaxTree a)
convertToBackendSyntaxTree (Sin a) = Unary Abs_ (convertToBackendSyntaxTree a)
convertToBackendSyntaxTree (Neg a) = Unary Neg_ (convertToBackendSyntaxTree a)
convertToBackendSyntaxTree (Length a) = Unary Length_ (convertToBackendSyntaxTree a)
convertToBackendSyntaxTree (Add a b) = Binary Add_ (convertToBackendSyntaxTree a) (convertToBackendSyntaxTree b)
convertToBackendSyntaxTree (Sub a b) = Binary Sub_ (convertToBackendSyntaxTree a) (convertToBackendSyntaxTree b)
convertToBackendSyntaxTree (Mul a b) = Binary Mul_ (convertToBackendSyntaxTree a) (convertToBackendSyntaxTree b)
convertToBackendSyntaxTree (Div a b) = Binary Div_ (convertToBackendSyntaxTree a) (convertToBackendSyntaxTree b)


mget :: Maybe Int -> Int
mget (Just a) = a
mget (Nothing) = 0

incVal :: AST -> Map AST Int -> Int
incVal key map = mget $ Map.lookup key map

add key hashmap = insert key (incVal key hashmap) hashmap

countSubTrees :: AST -> Map AST Int -> Map AST Int
countSubTrees ast hashmap          = insert ast 1 hashmap
countSubTrees (Unary _ a) hashmap   = add a hashmap
countSubTrees (Binary _ a b) hashmap = add b (add a hashmap) -- insert b (incVal map) (insert a (incVal map) map)
countSubTrees _ hashmap            = hashmap

countSubTrees' ast = countSubTrees ast (empty :: Map AST Int)

main = putStrLn $ glslLayer layer

