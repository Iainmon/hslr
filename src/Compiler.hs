module Compiler where
import SyntaxTree
import Optimizer
import Language
import CodeGeneration
import Data.List (foldl')

compile :: SyntaxTree -> String
compile (Imparitive instructions ast) = (foldl' (++) "" $ map (++";\n") $ map show instructions) ++ compile ast
compile ast = "\ncolor = " ++ show ast ++ ";\n"

generateProgram :: SyntaxTree -> String
generateProgram ast = foldl' (++) "" $ map (++"\n") sourceParts where
    sourceParts = [
        "#include \"lib/runtime.glsl\""
        ,"#include \"lib/noise.glsl\""
        -- ,"out vec4 gl_FragColor;"
        -- ,"vec3 program(void);"
        -- ,"void main () { gl_FragColor = program(); } "
        ,"void program(inout vec3 color) { \n" ++ indentedFunctionBody ++ "}"
        ] where indentedFunctionBody = unlines $ map ("\t"++) $ lines $ compile ast