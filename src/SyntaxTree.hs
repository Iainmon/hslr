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


module SyntaxTree where
-- (lit, var, SyntaxTree, compile, generateProgram, wrap, cosine, optimize, GLiteral,  Type(..))
import qualified Data.Map as M
import qualified Data.Bifunctor as BF
import Text.Printf
import qualified Data.List as List
import Data.Sort

data Type = Void | Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq,Ord)

-- data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq, Ord)
data AST = Call String [SyntaxTree] | Id String deriving (Eq,Ord)

data RoseTree a = RT Int [a]

-- Same thing
type Routine = SyntaxTree -> SyntaxTree
type Function = Routine

-- type Arithmatic = forall t. Num t

data SyntaxTree = 
     Node Type AST
    | Imparitive [SyntaxTree] SyntaxTree
    | Assignment Type String SyntaxTree deriving (Eq)

class Magnitude a where
    magnitude :: a -> Int


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


-- instance Functor CallType where
--     fmap f (Unary a) = Unary $ fmap f a
--     fmap f (Binary args) = Binary $ tupleMap (fmap f) args
--     fmap f (Trinary args) = Trinary $ tupleMap (fmap f) args

-- instance Functor SyntaxTree where
--     fmap f (Node type' (Call name call) = Node type' $ Call name $ fmap f call
--     fmap f (Node type' (Id name)) = 


-- instance Magnitude CallType where
--     magnitude (Unary a) = (1+) $ map magnitude a

instance Magnitude SyntaxTree where
    magnitude (Node _ (Call _ args)) = foldr (+) 1 $ map magnitude args
    magnitude (Node _ (Id _ ))   = 1
    magnitude (Imparitive _ _)   = 0
    magnitude (Assignment _ _ _) = 0

instance Ord SyntaxTree where
    compare a b = compare (magnitude a) (magnitude b)

class Accept a where
    preAccept :: (a -> a) -> a -> a
    postAccept :: (a -> a) -> a -> a
    accept :: (a -> a) -> a -> a

instance Accept SyntaxTree where
    accept f (Node type' (Call name args)) = Node type' $ Call name $ map f args
    accept f (Node type' (Id name)) = f $ Node type' $ Id name
    accept f (Imparitive assignments ast) = Imparitive (map (f) assignments) (f ast)
    accept f (Assignment type' name ast) = Assignment type' name $ f ast


    -- postAccept f (Node type' (Call name args)) = f $ Node type' $ Call name $ map (postAccept f) args
    -- postAccept f (Node type' (Id name)) = f $ Node type' $ Id name
    -- postAccept f (Imparitive assignments ast) = f $ Imparitive (map (postAccept f) assignments) (postAccept f ast)
    -- postAccept f (Assignment type' name ast) = f $ Assignment type' name $ postAccept f ast

    postAccept f (Node type' (Id name)) = f $ Node type' $ Id name
    postAccept f ast = f $ accept (postAccept f) ast

    preAccept f (Node type' (Id name)) = f $ Node type' $ Id name
    preAccept f ast = accept (preAccept f) $ f ast

-- instance Functor SyntaxTree where
--     fmap f (Node type' (Call name args)) = f $ Node type' $ Call name $ map (fmap f) args
--     fmap f (Assignment type' name ast) = f $ Assignment type' name $ fmap f ast
--     fmap f (Imparitive assignments ast) = f $ Imparitive (map (fmap f) assignments) (fmap f ast)

-- instance Functor AST where
--     fmap f (Call name args) = f $ Call name $ map (fmap f) args