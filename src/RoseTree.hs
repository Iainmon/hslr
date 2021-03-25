
module RoseTree where 
import Data.List as List

-- data TypedSyntaxNode t = Node t String deriving (Eq,Show)
data RoseTree a = Leaf a | Branch a [RoseTree a] deriving (Eq,Show)

instance Functor RoseTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch a as) = Branch (f a) $ map (fmap f) as

instance Foldable RoseTree where
    foldr f z (Leaf a) = a `f` z
    foldr f z (Branch a as) = a `f` (List.foldl (foldr f) z as)

-- rt = Branch 1 [Leaf 2, Branch 3 [Leaf 4, Leaf 5]]