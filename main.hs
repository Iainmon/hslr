{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Control.Monad (when)
import Prelude hiding (cos)

color = var Float "color"
uv = var Float "uv"
uv_x = var Float "uv.x"
time = var Float "u_time"
cos = cosine
program = vector (c * (lit 0.6),c * (lit 0.3),c * (lit 0.5)) where c = cos $ time * (lit 0.4) + (uv_x * 30)


main = do
    let newContents = generateProgram program
    when (length newContents > 0) $
        writeFile "out.frag" newContents