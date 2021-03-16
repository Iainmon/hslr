
    
import SyntaxTree
import Prelude hiding ((*))

scale = lit (10 :: Float)
position = var Vector2 "uv"
ihat = lit ((1,0) :: (Int,Int))
jhat = lit ((1,0) :: (Int,Int))

ast = scale * position * position * jhat * ihat


main = putStrLn $ show ast