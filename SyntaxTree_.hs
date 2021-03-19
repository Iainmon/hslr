-- {-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, RankNTypes #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

====3
module SyntaxTree_ where
-- (lit, var, SyntaxTree, compile, generateProgram, wrap, cosine, optimize, GLiteral,  Type(..))
-- import Prelude hiding ((*))
import qualified Data.Map as M
import qualified Data.Bifunctor as BF
import Text.Printf
import qualified Data.List as List
import Data.Sort


data Type = Void | Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq,Ord)

data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq, Ord)
data AST = Call String CallType | Id String deriving (Eq,Ord)

-- type Arithmatic = forall t. Num t

data SyntaxTree = 
     Node Type AST
    | Imparitive [SyntaxTree] SyntaxTree
    | Assignment Type String SyntaxTree deriving (Eq)

class GLiteral t where
    represent :: t -> String
    syntaxNode :: t -> SyntaxTree


instance GLiteral Int where
    represent = show
    syntaxNode n = Node Float $ Id $ represent n
instance GLiteral Integer where
    represent = show
    syntaxNode n = Node Float $ Id $ represent n
instance GLiteral Float where
    represent = show
    syntaxNode n = Node Float $ Id $ represent n
instance GLiteral Double where
    represent = show
    syntaxNode n = Node Float $ Id $ represent n

instance GLiteral (String,Type) where
    represent (name, _) = name
    syntaxNode (name, type') = Node type' $ Id name

instance GLiteral SyntaxTree where
    represent = show
    syntaxNode n = n

instance (GLiteral a, b ~ a) => GLiteral (a,b) where
    represent (a,b) = show $ syntaxNode (a,b)
    syntaxNode (a,b) = Node Vector2 $ Call "vec2" $ Binary (syntaxNode a, syntaxNode b)
instance (GLiteral a, b ~ a, c ~ a) => GLiteral (a,b,c) where
    represent (a,b,c) = show $ syntaxNode (a,b,c)
    syntaxNode (a,b,c) = Node Vector2 $ Call "vec3" $ Trinary (syntaxNode a, syntaxNode b, syntaxNode c)


instance Show Type where
    show Int = "int"
    show Float = "float"
    show Bool = "bool"
    show Vector2 = "vec2"
    show Vector3 = "vec3"
    show Vector4 = "vec4"
    show Void = "void"

tupleMap f = BF.bimap f f

-- instance Functor CallType where
--     fmap f (Unary a) = Unary $ fmap f a
--     fmap f (Binary args) = Binary $ tupleMap (fmap f) args
--     fmap f (Trinary args) = Trinary $ tupleMap (fmap f) args

-- instance Functor SyntaxTree where
--     fmap f (Node type' (Call name call) = Node type' $ Call name $ fmap f call
--     fmap f (Node type' (Id name)) = 


class Magnitude a where
    magnitude :: a -> Int

instance Magnitude CallType where
    magnitude (Unary a) = 1 + magnitude a
    magnitude (Binary (a,b))  = foldr (+) 1 $ map magnitude [a,b]
    magnitude (Trinary (a,b,c)) = foldr (+) 1 $ map magnitude [a,b,c]

instance Magnitude SyntaxTree where
    magnitude (Node _ (Call _ call)) = magnitude call
    magnitude (Node _ (Id _ ))   = 1
    magnitude (Imparitive _ _)   = 0
    magnitude (Assignment _ _ _) = 0

-- START works
-- comparisonFromOrdering :: (Ord e, Eq a) => [a] -> (e -> a) -> e -> e -> Ordering
-- comparisonFromOrdering ordering f a b = unwrap $ (idxA >>= (\a -> idxB >>= Just . (compare a)))
--     where idxA = List.elemIndex (f a) ordering
--           idxB = List.elemIndex (f b) ordering
--           unwrap (Just x) = x
--           unwrap Nothing = EQ

-- instance Ord Type where
--     compare = comparisonFromOrdering typeOrdering id
--         where typeOrdering = [Void, Bool, Int, Float, Vector2, Vector3, Vector4]

-- data ConsAST = Call_ | Id_ deriving (Eq)
-- instance Ord AST where
--     -- compare (Call _ _) (Id _) = GT
--     -- compare a b = compare b a
--     compare = comparisonFromOrdering typeOrdering (extract :: AST -> ConsAST)
--         where typeOrdering = [Id_, Call_]
--               extract (Call _ _) = Call_
--               extract (Id _) = Id_
-- END words

instance Ord SyntaxTree where
    compare a b = compare (magnitude a) (magnitude b)


-- instance Ord CallType where
--     compare (lhsCons lhsArgs) (rhsCons rhsArgs) = BF.bimap 

-- instance Ord SyntaxTree where

-- instance Ord CallType where




parenthisize' :: [String] -> String
parenthisize' (a:[]) = a
parenthisize' (a:as) = a ++ ", " ++ parenthisize' as
parenthisize' [] = ""

parenthisize as = "(" ++ parenthisize' as ++ ")"

instance Show SyntaxTree where
    show (Node _ ast) = show ast
    show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldr (++) "" $ map (++";\n") $ map show assignments
    show (Assignment ty name a) = show ty ++ " " ++ name ++ " = " ++ show a


instance Show CallType where
    show (Unary (a)) = parenthisize $ map show [a]
    show (Binary (a,b)) = parenthisize $ map show [a,b]
    show (Trinary (a,b,c)) = parenthisize $ map show [a,b,c]

isInfix :: AST -> Maybe String
isInfix (Call "add" _) = Just ['+']
isInfix (Call "mul" _) = Just ['*']
isInfix (Call "sub" _) = Just ['-']
isInfix (Call "div" _) = Just ['/']
isInfix _ = Nothing

parensWrap "" = ""
parensWrap a  = "(" ++ a ++ ")"

instance Show AST where
    show (Id a) = a
    -- show (Assignment t name a) = show t ++ " " ++ name ++ " = " ++ show a
    -- show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldl (++) "" $ map (++";\n") $ map show assignments
    -- show (Call name (Unary (a)))       = name ++ (parenthisize $ map show [a])
    -- show (Call name (Binary (a,b)))    = name ++ (parenthisize $ map show [a,b])
    -- show (Call name (Trinary (a,b,c))) = name ++ (parenthisize $ map show [a,b,c])
    show (Call name call) = shownCall where
        shownCall = case isInfix (Call name call) of
                Just operator -> showBinaryInfix operator call
                Nothing       -> name ++ show call
                where showBinaryInfix operator (Binary (lhs, rhs)) = parensWrap $ show lhs ++ " " ++ operator ++ " " ++ show rhs

wrap :: (GLiteral a) => (SyntaxTree -> SyntaxTree) -> a -> SyntaxTree
wrap f = f . syntaxNode

makeUndefinedError funcName ast = "Error! No implementation of " ++ funcName ++ " accepts " ++ show ast

cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine (Node Vector2 x) = Node Vector2 $ Call "cos" $ Unary (Node Vector2 x)
cosine x = error $ "No definition for " ++ show x

-- add (Node consa asta) (Node consb astb) = Node consa $ Call "add" $ Binary (Node consa asta, Node consb astb)
-- vecInc (Node Vector2 v) = add (Node Vector2 v) (syntaxNode ((1,1) :: (Int,Int)))


var :: Type -> String -> SyntaxTree
var ty name = Node ty $ Id name
-- var = syntaxNode


iden :: String -> AST
iden s = Id s

lit :: (GLiteral a) => a -> SyntaxTree
lit = syntaxNode


-- vector2 :: SyntaxTree -> SyntaxTree -> SyntaxTree
-- vector2 (Node Float x) (Node Float y) = Node Vector2 $ Call "vec2" $ Binary (Node Float x) (Node Float y)

ast = var Float "iain"



mget :: Maybe Int -> Int
mget (Just a) = a
mget (Nothing) = 0

incVal :: SyntaxTree -> M.Map SyntaxTree Int -> Int
incVal key map_ = (1+) $ mget $ M.lookup key map_

add :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
add key hashmap = M.insert key (incVal key hashmap) hashmap

countSubTrees :: SyntaxTree -> M.Map SyntaxTree Int -> M.Map SyntaxTree Int
-- countSubTrees (Unary _ a) hashmap   = countSubTrees a $ add a hashmap
-- countSubTrees (Binary _ a b) hashmap = countSubTrees b $ add b $ countSubTrees a (add a hashmap) -- insert b (incVal map) (insert a (incVal map) map)
-- countSubTrees (Id_ _) hashmap            = hashmap
-- countSubTrees ast hashmap          = M.insert ast 1 hashmap

-- countSubTrees (Imparitive _ ast) hashmap = countSubTrees ast hashmap
-- countSubTrees (Node type' (Call name (Unary a))) hashmap         = countSubTrees a $ flip add hashmap (Node type' (Call name (Unary a)))
-- countSubTrees (Node type' (Call name (Binary (a,b)))) hashmap    = countSubTrees b $ add b $ countSubTrees a $ add a $ flip add hashmap (Node type' (Call name (Binary (a,b))))
-- countSubTrees (Node type' (Call name (Trinary (a,b,c)))) hashmap = countSubTrees c $ add c $ countSubTrees b $ add b $ countSubTrees a $ add a $ flip add hashmap (Node type' (Call name (Trinary (a,b,c))))
-- countSubTrees (Node type' (Id name)) hashmap                     = hashmap
-- countSubTrees ast hashmap                                        = if not $ M.member ast hashmap then M.insert ast 1 hashmap else hashmap


-- Not adding child nodes into the hasmamp without visiting them
countSubTrees (Imparitive _ ast) hashmap = countSubTrees ast hashmap
countSubTrees (Node type' (Call name (Unary a))) hashmap         = countSubTrees a $ flip add hashmap (Node type' (Call name (Unary a))) 
-- countSubTrees (Node type' (Call name (Unary a))) hashmap         = add (Node type' (Call name (Unary a))) $ countSubTrees a hashmap
countSubTrees (Node type' (Call name (Binary (a,b)))) hashmap    = countSubTrees b $ countSubTrees a $ flip add hashmap (Node type' (Call name (Binary (a,b))))
countSubTrees (Node type' (Call name (Trinary (a,b,c)))) hashmap = countSubTrees c $ countSubTrees b $ countSubTrees a $ flip add hashmap (Node type' (Call name (Trinary (a,b,c))))
countSubTrees (Node type' (Id name)) hashmap                     = hashmap
countSubTrees ast hashmap                                        = if not $ M.member ast hashmap then M.insert ast 1 hashmap else hashmap




-- countSubTrees (Imparitive _ ast) hashmap = countSubTrees ast hashmap
-- countSubTrees (Node _ (Call _ (Unary a))) hashmap         = countSubTrees a $ add a hashmap
-- countSubTrees (Node _ (Call _ (Binary (a,b)))) hashmap    = countSubTrees b $ add b $ countSubTrees a (add a hashmap)
-- countSubTrees (Node _ (Call _ (Trinary (a,b,c)))) hashmap = countSubTrees c $ add c $ countSubTrees b $ add b $ countSubTrees a $ add a hashmap
-- countSubTrees (Node _ (Id _)) hashmap                     = hashmap
-- countSubTrees ast hashmap                                 = if not $ M.member ast hashmap then M.insert ast 1 hashmap else hashmap


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
-- factor (Node type' (Call name (Unary arg))) (term,n)    = if (Node type' (Call name (Unary arg))) == term
--                                                             then Node type' $ Id $ cacheSymbol n
--                                                             else Node type' $ Call name $ Unary $ factor arg (term,n)
-- factor (Node type' (Call name (Binary args))) (term,n)  = if (Node type' (Call name (Binary args))) == term
--                                                             then Node type' $ Id $ cacheSymbol n
--                                                             else Node type' $ Call name $ Binary $ BF.bimap f f args where f = (flip factor (term,n))
-- factor (Node type' (Call name (Trinary args))) (term,n) = if (Node type' (Call name (Trinary args))) == term
--                                                             then Node type' $ Id $ cacheSymbol n
--                                                             else Node type' $ Call name $ Trinary $ BF.bimap f f args where f = (flip factor (term,n))
-- factor x _ = x
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





-- factor ast term = if ast == term then (Id_ "0x69") else factor ast term

precidence typeA typeB = typeA

instance Num SyntaxTree where
    -- (*) :: SyntaxTree -> SyntaxTree -> SyntaxTree
    (*) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "mul" $ Binary (Node lhsType lhs, Node rhsType rhs)
    -- (+) :: SyntaxTree -> SyntaxTree -> SyntaxTree
    (+) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "add" $ Binary (Node lhsType lhs, Node rhsType rhs)
    (-) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "sub" $ Binary (Node lhsType lhs, Node rhsType rhs)
    abs (Node type' node) = Node type' $ Call "abs" $ Unary $ Node type' node
    signum = id
    fromInteger n = syntaxNode n

wrapPureAST :: SyntaxTree -> SyntaxTree
wrapPureAST (Imparitive instructions ast) = Imparitive instructions ast
wrapPureAST ast = Imparitive [] ast

appendInstructions :: SyntaxTree -> [SyntaxTree] -> SyntaxTree
appendInstructions (Imparitive instructions ast) newInstructions = Imparitive (instructions ++ newInstructions) ast
appendInstructions ast newInstructions = flip appendInstructions newInstructions $ wrapPureAST ast

appendInstruction ast newInstruction = appendInstructions ast [newInstruction]


factor' = factor . wrapPureAST

tag [] _ = []
tag (x:xs) n = (x,n) : tag xs (n+1)

cachedOperationAssignments :: [(SyntaxTree, Int)] -> [SyntaxTree]
cachedOperationAssignments taggedSubTrees = map makeAssignment taggedSubTrees where
    makeAssignment ((Node type' subtree), n) = Assignment type' ("cahce_" ++ hexShow n) (Node type' subtree)

optimize :: SyntaxTree -> SyntaxTree
-- optimize ast = foldl factor' ast $ subtreesToOptimize $ M.toList $ countSubTrees' ast
optimize ast = foldr (flip factor) programTree $ flip tag 1 $ reverse $ sort $ subtreesToOptimize occurances
    where occurances = M.toList $ invertHashMap $ invertHashMap $ countSubTrees' ast
          programTree = wrapPureAST ast
-- optimize ast = foldl factor (wrapPureAST ast) $ M.toList $ invertHashMap $ invertHashMap $ countSubTrees' ast

hexShow n = printf "0x%x" n

compile :: SyntaxTree -> String
compile (Imparitive instructions ast) = (foldl (++) "" $ map (++";\n") $ map show instructions) ++ compile ast
compile ast = "vec3 color;\ncolor = " ++ show ast ++ ";\nreturn color;\n"

generateProgram :: SyntaxTree -> String
generateProgram ast = foldr (++) "" $ map (++"\n") sourceParts where
    sourceParts = [
        "#include \"HaskellSHadingLanguage.glsl\""
        ,"out vec4 gl_FragColor;"
        ,"vec3 program(void);"
        ,"void main () { gl_FragColor = program(); } "
        ,"vec3 program(void) { \n" ++ indentedFunctionBody ++ "}"
        ] where indentedFunctionBody = unlines $ map ("\t"++) $ lines $ compile ast