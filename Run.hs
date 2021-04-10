module Run where

import System.Process
import Control.Monad (when)


data RunParams = Params {
      width :: Int
    , height :: Int
    , fileName :: String
    }

instance Show RunParams where
    show ps = " -w " ++ show (width ps) ++ " -h " ++ show (height ps) ++ " " ++ fileName ps

run params = do
    callCommand $ "glslViewer " ++ show params

save source fileName = do
    when (length source > 0) $
        writeFile fileName source