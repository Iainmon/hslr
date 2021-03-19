{-# LANGUAGE ExtendedDefaultRules #-}
import SyntaxTree
import Control.Monad (when)
import Prelude hiding (cos)
import Data.Sort
-- program = cosine $ wrap cosine 1


-- main = putStrLn $ generateProgram $ optimize $ program
color = var Float "color"
uv = var Float "uv"
cos = cosine
comp f = cos . f . cos . f . cos . f . cos

squaredUV = uv * uv
cosSum = ((cosine squaredUV) + (cosine squaredUV)) + ((cosine squaredUV) + (cosine squaredUV))
-- program = cosSum + cosSum * (cosine cosSum) + (comp cos uv) + (comp cos $ comp cosine uv)
program = (cos (color * color)) + (cos (color * color))

programA = generateProgram program
programB = generateProgram $ optimize program
programC = (generateProgram . optimize . optimize . optimize) program
-- main = putStrLn $ generateProgram $ optimize $ optimize $ program
-- main = do
--           mapM_ putStrLn $ [programA, programB, programC]
--           print $ programC == programB


main = do
    let newContents = programB
    when (length newContents > 0) $
        writeFile "out.glsl" newContents