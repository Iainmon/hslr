{-# LANGUAGE FlexibleInstances, TypeFamilies #-}


module SyntaxTree where
-- (lit, var, SyntaxTree, compile, generateProgram, wrap, cosine, optimize, GLiteral,  Type(..))
import qualified Data.Map as M
import qualified Data.Bifunctor as BF
import Text.Printf
import qualified Data.List as List
import Data.Sort
import RoseTree

data Type = Void | Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq,Ord)

-- data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq, Ord)
data AST = Call String [SyntaxTree] | Id String deriving (Ord)
-- data TypeableAST t = Call String [SyntaxTree] | Id String deriving (Eq,Ord)
-- type AST = TypeableAST Type

-- Same thing
type Routine = SyntaxTree -> SyntaxTree
type Function = Routine

-- type Arithmatic = forall t. Num t

data SyntaxTree = 
     Node Type AST
    | Imparitive [SyntaxTree] SyntaxTree
    | Assignment Type String SyntaxTree deriving (Eq)

-- data TypeableSyntaxTree t = Node t (TypeableAST t) | Imparitive [TypeableSyntaxTree t] (TypeableSyntaxTree t) | Assignment t String (TypeableSyntaxTree t) deriving (Eq)
-- type SyntaxTree = TypeableSyntaxTree Type

data SyntaxTree' = SyntaxNode { expType :: Type, identifier :: String, args :: Maybe [SyntaxTree']}


tupleMap f = BF.bimap f f

typeof :: SyntaxTree -> Type
typeof (Node type' _) = type'
typeof _ = Void

unwrapTypedNode :: SyntaxTree -> Maybe AST
unwrapTypedNode (Node _ ast) = Just ast
unwrapTypedNode (Assignment _ _ _) = Nothing
unwrapTypedNode (Imparitive _ _) = Nothing

nameof :: AST -> String
nameof (Call name _) = name
nameof (Id name) = name


instance Magnitude SyntaxTree where
    magnitude (Node _ (Call _ args)) = foldr (+) 1 $ map magnitude args
    magnitude (Node _ (Id _ ))   = 1
    magnitude (Imparitive _ _)   = 0
    magnitude (Assignment _ _ _) = 0

instance Ord SyntaxTree where
    compare a b = compare (magnitude a) (magnitude b)

pairEquality :: (Eq a) => (a,a) -> Bool
pairEquality (a,b) = a == b


instance Eq AST where
    (==) (Call name args) (Call name' args') = (name == name') `and` (foldr and True $ map pairEquality $ zip args args')
        where and = (&&)
    (==) (Id name) (Id name') = name == name'
    (==) _ _ = False

class Accept a where
    preAccept :: (a -> a) -> a -> a
    postAccept :: (a -> a) -> a -> a
    accept :: (a -> a) -> a -> a

instance Accept SyntaxTree where
    accept f (Node type' (Call name args)) = Node type' $ Call name $ map f args
    accept f (Node type' (Id name)) = f $ Node type' $ Id name
    accept f (Imparitive assignments ast) = Imparitive (map (f) assignments) (f ast)
    accept f (Assignment type' name ast) = Assignment type' name $ f ast

    postAccept f (Node type' (Id name)) = f $ Node type' $ Id name
    postAccept f ast = f $ accept (postAccept f) ast

    preAccept f (Node type' (Id name)) = f $ Node type' $ Id name
    preAccept f ast = accept (preAccept f) $ f ast



-- data SynTree a = SynTree SyntaxTree
-- instance Foldable SynTree where
--     foldr f z (SynTree (Node type' (Id name))) = (Node type' (Id name)) `f` z
--     foldr f z (SynTree (Node type' (Call name args))) = (Node type' (Call name args)) `f` (List.foldr (foldr f z) z $ map SynTree args)

-- instance Functor TypeableSyntaxTree where
--     fmap f (Node type' (Call name args)) = Node type' $ Call name $ map f args
--     fmap f (Node type' (Id name)) = f $ Node type' $ Id name
--     fmap f (Imparitive assignments ast) = Imparitive (map (f) assignments) (f ast)
--     fmap f (Assignment type' name ast) = Assignment type' name $ f ast

