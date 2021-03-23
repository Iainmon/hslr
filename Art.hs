{-# LANGUAGE ExtendedDefaultRules #-}

module Art where

import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,abs,(.),(^))
import qualified Prelude (length)


data Layer = Color SyntaxTree | Combination Layer Layer SyntaxTree

color (a,b,c) = Color $ vector (a,b,c)

_uv = (var Vector2 "uv") - (vector (0.5,0.5))

circle r = smoothstep r (r+0.02) (root $ (_uv.x * _uv.x) + (_uv.y * _uv.y))
spinny x y = x^10 - 5/2*x^8 + 35/16*x^6 - 25/32*x^4 + 25/256*x^2 + 1/16*y^6 - 3/32*y^4 + 9/256*y^2 - 1/256

compileLayers (Color a) = a
compileLayers (Combination l1 l2 ast) = mix (compileLayers l1) (compileLayers l2) ast