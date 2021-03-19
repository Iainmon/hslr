{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Compiler where
import SyntaxTree
import Optimizer
import Language
import CodeGeneration

makeUndefinedError funcName ast = "Error! No implementation of " ++ funcName ++ " accepts " ++ show ast

cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine (Node Vector2 x) = Node Vector2 $ Call "cos" $ Unary (Node Vector2 x)
cosine x = error $ "No definition for " ++ show x

-- add (Node consa asta) (Node consb astb) = Node consa $ Call "add" $ Binary (Node consa asta, Node consb astb)
-- vecInc (Node Vector2 v) = add (Node Vector2 v) (syntaxNode ((1,1) :: (Int,Int)))

var :: Type -> String -> SyntaxTree
var ty name = Node ty $ Id name
-- var = syntaxNode


iden :: String -> AST
iden s = Id s

lit :: (GLiteral a) => a -> SyntaxTree
lit = syntaxNode

-- vector2 :: SyntaxTree -> SyntaxTree -> SyntaxTree
-- vector2 (Node Float x) (Node Float y) = Node Vector2 $ Call "vec2" $ Binary (Node Float x) (Node Float y)

ast = var Float "iain"

compile :: SyntaxTree -> String
compile (Imparitive instructions ast) = (foldl (++) "" $ map (++";\n") $ map show instructions) ++ compile ast
compile ast = "vec3 color;\ncolor = " ++ show ast ++ ";\nreturn color;\n"

generateProgram :: SyntaxTree -> String
generateProgram ast = foldr (++) "" $ map (++"\n") sourceParts where
    sourceParts = [
        "#include \"HaskellSHadingLanguage.glsl\""
        ,"out vec4 gl_FragColor;"
        ,"vec3 program(void);"
        ,"void main () { gl_FragColor = program(); } "
        ,"vec3 program(void) { \n" ++ indentedFunctionBody ++ "}"
        ] where indentedFunctionBody = unlines $ map ("\t"++) $ lines $ compile ast