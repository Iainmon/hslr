{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Optimizer where
import SyntaxTree
import CodeGeneration
import Language
import qualified Data.Map as M
import qualified Data.Bifunctor as BF
import Text.Printf
import qualified Data.List as List
import Data.Sort


mget :: Maybe Int -> Int
mget (Just a) = a
mget (Nothing) = 0

incVal :: SyntaxTree -> M.Map SyntaxTree Int -> Int
incVal key map_ = (1+) $ mget $ M.lookup key map_

add :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
add key hashmap = M.insert key (incVal key hashmap) hashmap

countSubTrees :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
countSubTrees (Imparitive _ ast) hashmap = countSubTrees ast hashmap
countSubTrees (Node type' (Call name (Unary a))) hashmap         = countSubTrees a $ flip add hashmap (Node type' (Call name (Unary a))) 
countSubTrees (Node type' (Call name (Binary (a,b)))) hashmap    = countSubTrees b $ countSubTrees a $ flip add hashmap (Node type' (Call name (Binary (a,b))))
countSubTrees (Node type' (Call name (Trinary (a,b,c)))) hashmap = countSubTrees c $ countSubTrees b $ countSubTrees a $ flip add hashmap (Node type' (Call name (Trinary (a,b,c))))
countSubTrees (Node type' (Id name)) hashmap                     = hashmap
countSubTrees ast hashmap                                        = if not $ M.member ast hashmap then M.insert ast 1 hashmap else hashmap

countSubTrees' ast = countSubTrees ast (M.empty :: M.Map SyntaxTree Int)

invertHashMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
invertHashMap hashmap = M.fromList $ map flipTwoTuple $ M.toList hashmap where
    flipTwoTuple (a,b) = (b,a)

first (x, _) = x
second (_, x) = x

subtreesToOptimize :: [(SyntaxTree, Int)] -> [SyntaxTree]
subtreesToOptimize ts = map first $ filter (\pair -> second pair > 1) ts

cacheSymbol :: Int -> String
cacheSymbol n = "_cache_" ++ hexShow n

factor :: SyntaxTree -> (SyntaxTree,Int) -> SyntaxTree
factor (Imparitive instructions ast) ((Node type' subtree),n) = Imparitive newInstructions $ factor ast (term,n)
    where term = (Node type' subtree)
          newInstruction = Assignment type' ("_cache_" ++ hexShow n) term
          newInstructions = if not $ elem term instructions then (newInstruction : instructions) else instructions
factor (Node type' (Id name)) _ = Node type' $ Id name
factor (Node type' (Call name (Unary arg))) (term,n)    = if (Node type' (Call name (Unary arg))) == term
                                                            then Node type' $ Id $ cacheSymbol n
                                                            else Node type' $ Call name $ Unary $ factor arg (term,n)
factor (Node type' (Call name (Binary args))) (term,n)  = if (Node type' (Call name (Binary args))) == term
                                                            then Node type' $ Id $ cacheSymbol n
                                                            else Node type' $ Call name $ Binary $ BF.bimap f f args where f = (flip factor (term,n))
factor (Node type' (Call name (Trinary args))) (term,n) = if (Node type' (Call name (Trinary args))) == term
                                                            then Node type' $ Id $ cacheSymbol n
                                                            else Node type' $ Call name $ Trinary $ BF.bimap f f args where f = (flip factor (term,n))
factor x _ = x

factor' = factor . wrapPureAST

tag [] _ = []
tag (x:xs) n = (x,n) : tag xs (n+1)

cachedOperationAssignments :: [(SyntaxTree, Int)] -> [SyntaxTree]
cachedOperationAssignments taggedSubTrees = map makeAssignment taggedSubTrees where
    makeAssignment ((Node type' subtree), n) = Assignment type' ("cahce_" ++ hexShow n) (Node type' subtree)

optimize :: SyntaxTree -> SyntaxTree
optimize ast = foldr (flip factor) programTree $ flip tag 1 $ reverse $ sort $ subtreesToOptimize occurances
    where occurances = M.toList $ invertHashMap $ invertHashMap $ countSubTrees' ast
          programTree = wrapPureAST ast

hexShow n = printf "0x%x" n
