module Spreadable where

class Spreadable a where
    spread :: a -> [a]
