{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,(.),abs)
import qualified Prelude (length)
color = var Float "color"
uv = uv' - vector (0.5,0.5) where uv' = var Float "uv"
time = var Float "u_time"
cos = cosine
sin = sine
norm = normalize
barCount = 100.0 * (sin time)
program = vector (c * 0.6 * (sine $ barCount * uv.y + time), (norm $ (cos uv.x) + (sin uv.y))/2, c * (root time))
        where col = (c * 0.6, c * 0.3, c * 0.5)
              -- c = cos $ time * 0.4 + (uv.x * 30)
              c = uv * uv * uv * uv + uv + uv + uv + uv

normOccilation = 0.5 * (sin time) + 0.5


biasPlane = smoothstep r (r+s) z
    where z = root $ (uv.x * uv.x) + (uv.y * uv.y)
          r = 0.2 + (normOccilation * 0.2) + (0.2 * perlin (uv + time)) + (0.2 * perlin (uv + time + 10.0))
          s = 0.02

yellow = vector (0.988,0.729,0.0117)
purple = vector (0.68, 0.23, 0.81)
program2 = mix col white biasPlane
    where col = yellow
          white = vector (1,1,1)


main = do
    let newContents = generateProgram  $ flattenCommutitiveOperations program2
    when (Prelude.length newContents > 0) $
        writeFile "out.frag" newContents