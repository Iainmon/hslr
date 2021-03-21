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

