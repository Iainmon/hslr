import qualified Data.Map as M
import Text.Printf

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

ast :: SyntaxTree
ast = Add (Mul (Mul (Cos (Id "uv")) (Cos (Id "uv"))) (Sin (Id "uv"))) (Id "1") -- cos(uv) * sin(uv) 

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

incVal :: AST -> M.Map AST Int -> Int
incVal key map_ = (1+) $ mget $ M.lookup key map_

add key hashmap = M.insert key (incVal key hashmap) hashmap

countSubTrees :: AST -> M.Map AST Int -> M.Map AST Int
countSubTrees (Unary _ a) hashmap   = countSubTrees a $ add a hashmap
countSubTrees (Binary _ a b) hashmap = countSubTrees b $ add b $ countSubTrees a (add a hashmap) -- insert b (incVal map) (insert a (incVal map) map)
countSubTrees ast hashmap          = M.insert ast 1 hashmap
countSubTrees _ hashmap            = hashmap

countSubTrees' ast = countSubTrees ast (M.empty :: M.Map AST Int)

first (x, _) = x
second (_, x) = x

subtreesToOptimize :: [(AST, Int)] -> [AST]
subtreesToOptimize ts = map first $ filter (\pair -> second pair > 1) ts

factor :: AST -> (AST,Int) -> AST
factor (Imparitive instructions ast) (term,n) = Imparitive ((Ass_ (hexShow n) term) : instructions) $ factor ast (term,n)
factor (Id_ a) _ = Id_ a
factor (Binary cons last rast) (term,n) = if (Binary cons last rast) == term then (Id_ (hexShow n)) else (Binary cons (factor last (term,n)) (factor rast (term,n)))
factor (Unary cons ast) (term,n) = if (Unary cons ast) == term then (Id_ (hexShow n)) else (Unary cons (factor ast (term,n)))
-- factor ast term = if ast == term then (Id_ "0x69") else factor ast term

wrapPureAST :: AST -> AST
wrapPureAST (Imparitive instructions ast) = Imparitive instructions ast
wrapPureAST ast = Imparitive [] ast

factor' = factor . wrapPureAST

tag [] _ = []
tag (x:xs) n = (x,n) : tag xs (n+1)

optimize :: AST -> AST
-- optimize ast = foldl factor' ast $ subtreesToOptimize $ M.toList $ countSubTrees' ast
optimize ast = foldl factor (wrapPureAST ast) $ flip tag 1 $ subtreesToOptimize $ M.toList $ countSubTrees' ast

hexShow n = printf "0x%x" n

compile :: AST -> String
compile (Imparitive instructions ast) = (foldl (++) "" $ map (++";\n") $ map show instructions) ++ compile ast
compile ast = "return " ++ show ast ++ ";\n"

ast' = convertToBackendSyntaxTree ast
ast'' = convertToBackendSyntaxTree $ Mul (Cos (Cos (Id "a"))) (Cos (Cos (Id "a")))
main = putStrLn $ glslLayer layer

