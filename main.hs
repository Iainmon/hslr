{-# LANGUAGE ExtendedDefaultRules #-}
import SyntaxTree


-- program = cosine $ wrap cosine 1


-- main = putStrLn $ generateProgram $ optimize $ program

uv = var Float "uv"

squaredUV = uv * uv
cosSum = (cosine squaredUV) + (cosine squaredUV) + (cosine squaredUV) + (cosine squaredUV)
program = cosSum + cosSum * (cosine cosSum)

main = putStrLn $ generateProgram $ optimize $ program