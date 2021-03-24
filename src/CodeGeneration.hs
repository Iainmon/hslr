{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module CodeGeneration where
import SyntaxTree

parenthisize' :: [String] -> String
parenthisize' (a:[]) = a
parenthisize' (a:as) = a ++ ", " ++ parenthisize' as
parenthisize' [] = ""

parenthisize as = "(" ++ parenthisize' as ++ ")"

instance Show SyntaxTree where
    show (Node _ ast) = show ast
    show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldr (++) "" $ map (++";\n") $ map show assignments
    show (Assignment ty name a) = show ty ++ " " ++ name ++ " = " ++ show a

instance Show Type where
    show Int = "int"
    show Float = "float"
    show Bool = "bool"
    show Vector2 = "vec2"
    show Vector3 = "vec3"
    show Vector4 = "vec4"
    show Void = "void"

isInfix :: AST -> Maybe String
isInfix (Call "add" _) = Just ['+']
isInfix (Call "mul" _) = Just ['*']
isInfix (Call "sub" _) = Just ['-']
isInfix (Call "div" _) = Just ['/']
isInfix (Call "swizzle" _) = Just ['.']
isInfix _ = Nothing

parensWrap "" = ""
parensWrap a  = "(" ++ a ++ ")"

joinInfixList :: String -> [String] -> String
joinInfixList op [] = ""
joinInfixList op (a:as) = foldl (\n -> \m -> n ++ " " ++ op ++ " " ++ m) a as

instance Show AST where
    show (Id a) = a
    show (Call name args) = shownCall where
        shownCall = case isInfix (Call name args) of
                Just operator -> showBinaryInfix operator args
                Nothing       -> name ++ (parenthisize $ map show args)
                where showBinaryInfix operator args = parensWrap $ joinInfixList operator $ map show args
