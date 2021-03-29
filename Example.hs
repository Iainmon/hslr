{-# LANGUAGE ExtendedDefaultRules #-}

module Example where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
import Prelude hiding (cos,sin,length,abs,(.),(^))
import qualified Prelude (length,(.))

color = var Float "color"
uv = (var Vector2 "uv") - vector (0.5,0.5)
time = var Float "u_time"
cos = cosine
sin = sine

slice f r s = smoothstep r (r+s) f

distanceField x y = x^12 + 6*x^10* y^2 - 5*x^10 + 15*x^8 * y^4 - 25*x^8*y^2 + 5*x^8 - 2*x^7 + 20*x^6* y^6 - 50*x^6*y^4 + 20*x^6* y^2 + 42*x^5* y^2 + 15*x^4*y^8 - 50*x^4* y^6 + 30*x^4* y^4 - 70*x^3* y^4 + 6*x^2* y^10 - 25*x^2* y^8 + 20*x^2* y^6 + 14*x* y^6 + y^12 - 5*y^10 + 5*y^8

yellow = vector (0.988,0.729,0.0117)
white = vector (1,1,1)

program = mix yellow white $ slice (distanceField (pos.x) (pos.y)) (sin time) 0.01
    where pos = uv * 10

main = do
    let programOutput = generateProgram $ flattenAssociativeOperations $ optimize program
    when (Prelude.length programOutput > 0) $
        writeFile "out.frag" programOutput