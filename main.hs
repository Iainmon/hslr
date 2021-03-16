{-# LANGUAGE ExtendedDefaultRules #-}
import SyntaxTree
import Control.Monad (when)

-- program = cosine $ wrap cosine 1


-- main = putStrLn $ generateProgram $ optimize $ program

uv = var Float "uv"

squaredUV = uv * uv
cosSum = ((cosine squaredUV) + (cosine squaredUV)) + ((cosine squaredUV) + (cosine squaredUV))
program = cosSum + cosSum * (cosine cosSum)

programA = generateProgram program
programB = generateProgram $ optimize $ program
programC = (generateProgram . optimize . optimize . optimize) program
-- main = putStrLn $ generateProgram $ optimize $ optimize $ program
-- main = do
--           mapM_ putStrLn $ [programA, programB, programC]
--           print $ programC == programB


main = do
    let newContents = programC
    when (length newContents > 0) $
        writeFile "out.glsl" newContents