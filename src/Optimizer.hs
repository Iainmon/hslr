{-# LANGUAGE TupleSections #-}

module Optimizer (
    optimize,
    flattenAssociativeOperations,
    fromRoseTree
) where
import SyntaxTree
import CodeGeneration
import Language
import qualified Data.Map as M
import Text.Printf
import qualified Data.List as List
import Data.List (foldl')
import Data.Sort
import RoseTree

mget :: Maybe Int -> Int
mget (Just a) = a
mget (Nothing) = 0

incVal :: SyntaxTree -> M.Map SyntaxTree Int -> Int
incVal key map_ = (1+) $ mget $ M.lookup key map_

add :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
add key hashmap = M.insert key (incVal key hashmap) hashmap

countSubTrees :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
countSubTrees (Imparitive _ ast) hashmap = countSubTrees ast hashmap
countSubTrees (Node type' (Call name args)) hashmap = foldl' (\hm -> \arg -> countSubTrees arg hm) start args where start = flip add hashmap (Node type' (Call name args))
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
cacheSymbol n = "_" ++ hexShow n

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
                     flip zip (map second identifiers) $ map (\targetNode -> foldl' (flip factorOutPair) (first targetNode) (reverse identifiers)) $ identifiers

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
    then Node type' $ Call name $ (++) regexNodes $ foldl' (++) [] $ map callerArguments redexNodes
    else Node type' $ Call name $ map flattenAssociativeOperations args
        where flattenedChildren = map flattenAssociativeOperations args
              redexNodes = filter ((Just name ==) . (fmap nameof) . unwrapTypedNode) flattenedChildren
              regexNodes = filter ((Just name /=) . (fmap nameof) . unwrapTypedNode) flattenedChildren
flattenAssociativeOperations (Node type' (Id name)) = Node type' $ Id name
flattenAssociativeOperations (Assignment type' name ast) = Assignment type' name $ flattenAssociativeOperations ast
flattenAssociativeOperations (Imparitive assignments ast) = Imparitive (map flattenAssociativeOperations assignments) (flattenAssociativeOperations ast)




-- accept :: (a -> a) -> a -> a



spread' = filter ((<) 1 . magnitude) . spread
identifyUniqueSubTrees = map first . countOccurances . spread'
repetativeSubTrees :: SyntaxTree -> [SyntaxTree]
repetativeSubTrees = map first . filter ((<) 1 . second) . countOccurances . spread'


countOccurances :: (Ord a) => [a] -> [(a,Int)]
countOccurances = M.toList . M.fromListWith (+) . map (, 1)

duplicateOccurances = map first . filter ((<) 1 . second)
uniqueOccurances = map first . filter ((==) 1 . second)


uniqueFeatures :: (Ord a, Spreadable a) => a -> [a]
uniqueFeatures = uniqueOccurances . countOccurances . spread

duplicateFeatures :: (Ord a, Spreadable a) => a -> [a]
duplicateFeatures = duplicateOccurances . countOccurances . spread

duplicateRoseTrees :: (Ord a) => RoseTree a -> [RoseTree a]
duplicateRoseTrees = duplicateOccurances . countOccurances . filter ((<) 1 . numKinder) . spread
uniqueRoseTrees :: (Ord a) => RoseTree a -> [RoseTree a]
uniqueRoseTrees = uniqueOccurances . countOccurances . filter ((<) 1 . numKinder) . spread


type HashLabel = String

-- This is slow
hashSubTree :: RoseTree Identifier -> HashLabel
hashSubTree = show . fromRoseTree

labeledTree :: RoseTree Identifier -> RoseTree HashLabel
labeledTree (Branch a as) = Branch (hashSubTree (Branch a as)) hashedChildren
    where hashedChildren = map labeledTree as
labeledTree (Leaf a) = Leaf $ hashSubTree $ Leaf a


-- rtOptimize rt = fmap (\srt -> if M.member srt hm then (Identifier Void "cached") else srt)
--     where hashTree = labeledTree rt
--           pairs = zip (spread hashTree) (spread rt)
--           hm = M.fromList pairs
--           dups = duplicateRoseTrees hashTree

rtOptimize rt = fmap (flip getHm hm) hashTree
    where hashTree = labeledTree rt
          pairs = zip (map root $ spread hashTree) (map root $ spread rt)
          dupPairs = zip (map root $ duplicateRoseTrees hashTree) (duplicateRoseTrees rt)
          cachedDups = map (\(k,v) -> (k,Identifier Void "cached")) dupPairs
          hm' = M.fromList pairs
          hm = flip M.union hm' $ M.fromList cachedDups
          getHm = M.lookup
        --   getHm key hm = case M.lookup key hm of 
        --                   Just a -> a
        --                   Nothing -> Identifier Void "somethingfailed"

rtOptimize' = pruneNothingRoseBuds . rtOptimize

fromRoseTree' :: RoseTree Identifier -> Maybe SyntaxTree
fromRoseTree' = Just . fromRoseTree

synTreeOpt' synTree = (rtOptimize' . toRoseTree $ synTree) >>= fromRoseTree'

onlyJusts :: [Maybe a] -> [a]
-- onlyJusts []  = []
-- onlyJusts (Nothing:as)  = onlyJusts as
-- onlyJusts ((Just a):as) = a : onlyJusts as
onlyJusts = map unJust . filter (not . null)
    where unJust (Just a) = a -- Partial function but that's ok.

pruneNothingRoseBuds :: RoseTree (Maybe a) -> Maybe (RoseTree a)
pruneNothingRoseBuds (Branch Nothing _) = Nothing
pruneNothingRoseBuds (Leaf Nothing) = Nothing
pruneNothingRoseBuds (Branch (Just a) as) = Just $ Branch a $ onlyJusts $ map pruneNothingRoseBuds as
pruneNothingRoseBuds (Leaf (Just a)) = Just $ Leaf a


instance Spreadable SyntaxTree where 
    spread (Node type' (Id name)) = [Node type' (Id name)]
    spread (Node type' (Call name args)) = (Node type' (Call name args)) : (foldl' (++) [] $ map spread args)
