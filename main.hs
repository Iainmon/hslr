{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,abs,(.))
import qualified Prelude (length)
color = var Float "color"
st = var Vector2 "uv"
uv = st - vector (0.5,0.5)
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


colorf l = 0.5 + (0.5 * ( cos $ 3.1415 + (l * 0.15) + vector (0.0,0.6,1.0) ))

biasPlane = smoothstep r (r+s) z
    where z = root $ (uv.x * uv.>x) + (uv.y * uv\.y)
          r = 0.2 + (normOccilation * 0.2) + (0.2 * perlin (uv + time)) + (0.2 * perlin (uv + time + 10.0))
          s = 0.02

bp f r s = smoothstep r (r+s) f

yellow = vector (0.988,0.729,0.0117)
purple = vector (0.68, 0.23, 0.81)
white = vector (1,1,1)

program2 = mix col white biasPlane
    where col = yellow
          white = colorf (time * 5)


program3 = mix col white biasPlane
    where col = yellow
          biasPlane = bp (perlin $ st * 10 + (perlin (20 * uv + time))) (normOccilation) 0.001

main = do
    let newContents = generateProgram $ optimize $ flattenCommutitiveOperations program3
    when (Prelude.length newContents > 0) $
        writeFile "out.frag" newContents
m = main