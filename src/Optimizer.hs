{-# LANGUAGE FlexibleInstances, TypeFamilies, TupleSections #-}

module Optimizer where
import SyntaxTree
import CodeGeneration
import Language
import qualified Data.Map as M
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
countSubTrees (Node type' (Call name args)) hashmap = foldl (\hm -> \arg -> countSubTrees arg hm) start args where start = flip add hashmap (Node type' (Call name args))
countSubTrees ast hashmap                           = if not $ M.member ast hashmap then M.insert ast 1 hashmap else hashmap

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
factor (Node type' (Call name args)) (term,n) = if (Node type' (Call name args)) == term
                                                then Node type' $ Id $ cacheSymbol n
                                                else Node type' $ Call name $ fmap f args where f = (flip factor (term,n))
factor x _ = x
factor' = factor . wrapPureAST


replaceSubTree' :: SyntaxTree -> SyntaxTree -> SyntaxTree -> SyntaxTree
replaceSubTree' old new subTree = if subTree == old then new else subTree
replaceSubTree :: SyntaxTree -> SyntaxTree -> SyntaxTree -> SyntaxTree
replaceSubTree old new = preAccept (replaceSubTree' old new) 

factor'' = replaceSubTree

constructTermIdPair :: (SyntaxTree, String) -> (SyntaxTree, SyntaxTree)
constructTermIdPair (term, name) = (term, var (typeof term) name)

maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

-- This only really works well if the terms are sorted in accending order
-- Yikes this is super slow
deflateTerms terms syntaxTree = (deflatedSyntaxTree,deflated)
    where deflatedSyntaxTree = foldr factorOutPair syntaxTree $ reverse deflated
          identifierNames = map cacheSymbol [1..length terms]
          identifiers = map constructTermIdPair $ zip terms identifierNames
          factorOutPair (term,iden) tree = if tree == term then tree else factor'' term iden tree
          deflated = -- flip map identifiers $ \i -> (foldr (\j -> factorOutPair j) (first i) (drop (mget $ List.elemIndex i identifiers) identifiers), second i)
                     flip zip (map second identifiers) $ map (\targetNode -> foldr factorOutPair (first targetNode) identifiers) $ identifiers

-- findRepetativeSubTrees ast = sort $ subtreesToOptimize occurances
--     where occurances = M.toList $ invertHashMap $ invertHashMap $ countSubTrees' ast
findRepetativeSubTrees = sort . repetativeSubTrees

optimize' ast = deflateTerms (findRepetativeSubTrees ast) ast

makeAssignment :: (SyntaxTree,SyntaxTree) -> SyntaxTree
makeAssignment ((Node type' subtree), (Node _ (Id name))) = Assignment type' name (Node type' subtree)

optimize'' (Imparitive assignments ast) = cacheDeflatedTerms $ optimize' ast
    where cacheDeflatedTerms (deflatedTree,deflatedTerms) = Imparitive (assignments ++ (map makeAssignment deflatedTerms)) deflatedTree

optimize :: SyntaxTree -> SyntaxTree
optimize = optimize'' . wrapPureAST

tag [] _ = []
tag (x:xs) n = (x,n) : tag xs (n+1)

cachedOperationAssignments :: [(SyntaxTree, Int)] -> [SyntaxTree]
cachedOperationAssignments taggedSubTrees = map makeAssignment taggedSubTrees where
    makeAssignment ((Node type' subtree), n) = Assignment type' ("cahce_" ++ hexShow n) (Node type' subtree)

hexShow n = printf "0x%x" n

-- Use abstract algebra to work this, rather than just defining cases

commOps = ["mul", "add", "sub"]

callerArguments :: SyntaxTree -> [SyntaxTree]
callerArguments (Node _ (Call _ args)) = args
callerArguments _ = []

flattenAssociativeOperations :: SyntaxTree -> SyntaxTree
flattenAssociativeOperations (Node type' (Call name args)) =
    if elem name commOps
    then Node type' $ Call name $ (++) regexNodes $ foldl (++) [] $ map callerArguments redexNodes
    else Node type' $ Call name $ map flattenAssociativeOperations args
        where flattenedChildren = map flattenAssociativeOperations args
              redexNodes = filter ((Just name ==) . (fmap nameof) . unwrapTypedNode) flattenedChildren
              regexNodes = filter ((Just name /=) . (fmap nameof) . unwrapTypedNode) flattenedChildren
flattenAssociativeOperations (Node type' (Id name)) = Node type' $ Id name
flattenAssociativeOperations (Assignment type' name ast) = Assignment type' name $ flattenAssociativeOperations ast
flattenAssociativeOperations (Imparitive assignments ast) = Imparitive (map flattenAssociativeOperations assignments) (flattenAssociativeOperations ast)




-- accept :: (a -> a) -> a -> a


spread' :: SyntaxTree -> [SyntaxTree]
spread' (Node type' (Id name)) = [Node type' (Id name)]
spread' (Node type' (Call name args)) = (Node type' (Call name args)) : (foldr (++) [] $ map spread args)
spread = filter ((<) 1 . magnitude) . spread'

countOccurances :: (Ord a) => [a] -> [(a,Int)]
countOccurances = M.toList . M.fromListWith (+) . map (, 1)
identifyUniqueSubTrees = map first . countOccurances . spread

repetativeSubTrees :: SyntaxTree -> [SyntaxTree]
repetativeSubTrees = map first . filter ((<) 1 . second) . countOccurances . spread