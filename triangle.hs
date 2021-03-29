{-# LANGUAGE ExtendedDefaultRules #-}

module Triangle where
-- import SyntaxTree
-- import Language
-- import Compiler
-- import Optimizer
-- import Runtime
import HSLR
import Control.Monad (when)
-- import Prelude hiding (cos,sin,length,abs,(.),(^),and,floor)
-- import qualified Prelude (length,(.))
import qualified Prelude
import Prelude (($),(>),(*),(-),(+))

uv = (var Vector2 "uv") - vector (0.5,0.5)
time = var Float "u_time"
pos = var Vector2 "gl_FragCoord"
cos = cosine
sin = sine

-- color = white * pixel
--     where sierpinski x y = if (x & y) == 0 then 1 else 0
--           pixel = sierpinski (x position) (y position)
--           white = vector (1,1,1)

program = vector (1,1,1)

main = do
    let programOutput = generateProgram $ flattenAssociativeOperations program
    when (Prelude.length programOutput > 0) $
        Prelude.writeFile "triangle.frag" programOutput