{-# LANGUAGE ExtendedDefaultRules #-}

module Triangle where
import SyntaxTree
import Language
import Compiler
import Optimizer
import Runtime
import Control.Monad (when)
-- import Prelude hiding (cos,sin,length,abs,(.),(^),and,floor)
-- import qualified Prelude (length,(.))
import qualified Prelude
import Prelude (($),(>),(*),(-),(+))

color = var Float "color"
uv = (var Vector2 "uv") - vector (0.5,0.5)
time = var Float "u_time"
pos = var Vector2 "gl_FragCoord"
cos = cosine
sin = sine

zero = int 0
one = int 1
white = vector (1,1,1)
sierpinski x y = if' (x & y `eq` 0) then' 1 else' 0
xpos = int $ floor $ pos.x
ypos = int $ pos.y
program = white * pix where pix = sierpinski xpos ypos

main = do
    let programOutput = generateProgram $ flattenAssociativeOperations program
    when (Prelude.length programOutput > 0) $
        Prelude.writeFile "triangle.frag" programOutput