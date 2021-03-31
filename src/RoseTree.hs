{-# LANGUAGE DeriveTraversable, DefaultSignatures, FlexibleInstances,FlexibleContexts #-}

module RoseTree where 
import Data.List as List
import Data.Foldable

-- data TypedSyntaxNode t = Node t String deriving (Eq,Show)
data RoseTree a = Leaf a | Branch a [RoseTree a] deriving (Show,Traversable) -- Traversable is needed for Foldable

root :: RoseTree a -> a
root (Branch a _) = a
root (Leaf a)     = a

kinder :: RoseTree a -> [RoseTree a]
kinder (Branch _ as) = as
kinder (Leaf _) = []

numKinder :: RoseTree a -> Int
numKinder = length . kinder

class Spreadable a where
    spread :: a -> [a]
    spread = flip (:) []

instance Spreadable [a] where
    spread [] = []
    spread xs = xs : (spread . tail $ xs)


class Magnitude a where
    magnitude :: a -> Int

instance Magnitude (RoseTree a) where
    magnitude = (+) 1 . foldl' (+) 0 . map magnitude . kinder


instance Eq a => Eq (RoseTree a) where
    (==) (Branch a as) (Branch b bs) = (a == b) `and` (as == bs)
        where and = (&&)
    (==) (Leaf a) (Leaf b) = a == b
    (==) _ _ = False

instance Eq a => Ord (RoseTree a) where
    compare a b = compare (magnitude a) (magnitude b)

-- class RussianDollFunctor f where
--     hmap :: (a -> b)


instance Spreadable (RoseTree a) where
    spread (Leaf a) = [Leaf a]
    spread (Branch a as) = (Branch a as) : (foldl' (++) [] $ map spread as)

instance Functor RoseTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch a as) = Branch (f a) $ map (fmap f) as

instance Foldable RoseTree where
    foldr f z (Leaf a) = a `f` z
    foldr f z (Branch a as) = a `f` (List.foldl' (foldr f) z as)
    toList = map root . spread
-- rt = Branch 1 [Leaf 2, Branch 3 [Leaf 4, Leaf 5]]