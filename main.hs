{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,(.))
import qualified Prelude (length)
color = var Float "color"
uv = var Float "uv"
time = var Float "u_time"
cos = cosine
sin = sine
norm = normalize
barCount = 100.0 * (sin time)
program = vector (c * 0.6 * (sine $ barCount * uv.y + time), (norm $ (cos uv.x) + (sin uv.y))/2, c * (root time))
        where col = (c * 0.6, c * 0.3, c * 0.5)
              c = cos $ time * 0.4 + (uv.x * 30)


main = do
    let newContents = generateProgram $ optimize program
    when (Prelude.length newContents > 0) $
        writeFile "out.frag" newContents