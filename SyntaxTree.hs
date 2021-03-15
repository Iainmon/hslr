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


module SyntaxTree (lit, var, (*), Type(..)) where

import Prelude hiding ((*))


data Type = Int | Float | Bool | Vector2 | Vector3 | Vector4 deriving (Eq)

data CallType = Unary (SyntaxTree) | Binary (SyntaxTree,SyntaxTree) | Trinary (SyntaxTree,SyntaxTree,SyntaxTree) deriving (Eq)
data AST = Call String CallType | Id String deriving (Eq)

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

parenthisize :: [String] -> String
parenthisize [] = ""
parenthisize (a:[]) = a ++ ")"
parenthisize (a:as) = "(" ++ a ++ "," ++ parenthisize as

instance Show SyntaxTree where
    show (Node _ ast) = show ast
    show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldl (++) "" $ map (++";\n") $ map show assignments
    show (Assignment ty name a) = show ty ++ " " ++ name ++ " = " ++ show a

instance Show CallType where
    show (Unary (a)) = "(" ++ (parenthisize $ map show [a])
    show (Binary (a,b)) = parenthisize $ map show [a,b]
    show (Trinary (a,b,c)) = parenthisize $ map show [a,b,c]

instance Show AST where
    show (Id a) = a
    -- show (Assignment t name a) = show t ++ " " ++ name ++ " = " ++ show a
    -- show (Imparitive assignments ast) = definitions ++ show ast where definitions = foldl (++) "" $ map (++";\n") $ map show assignments
    -- show (Call name (Unary (a)))       = name ++ (parenthisize $ map show [a])
    -- show (Call name (Binary (a,b)))    = name ++ (parenthisize $ map show [a,b])
    -- show (Call name (Trinary (a,b,c))) = name ++ (parenthisize $ map show [a,b,c])
    show (Call name call) = name ++ show call

wrap :: (GLiteral a) => (SyntaxTree -> SyntaxTree) -> a -> SyntaxTree
wrap f = f . syntaxNode






















cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine (Node Vector2 x) = Node Vector2 $ Call "cos" $ Unary (Node Vector2 x)
cosine _ = undefined

add (Node consa asta) (Node consb astb) = Node consa $ Call "add" $ Binary (Node consa asta, Node consb astb)

vecInc (Node Vector2 v) = add (Node Vector2 v) (syntaxNode ((1,1) :: (Int,Int)))

precidence consa consb = consa

(*) :: SyntaxTree -> SyntaxTree -> SyntaxTree
(*) (Node consa asta) (Node consb astb) = Node (precidence consa consb) $ Call "mul" $ Binary (Node consa asta, Node consb astb)

var :: Type -> String -> SyntaxTree
var ty name = Node ty $ Id name

iden :: String -> AST
iden s = Id s

lit :: (GLiteral a) => a -> SyntaxTree
lit = syntaxNode


-- vector2 :: SyntaxTree -> SyntaxTree -> SyntaxTree
-- vector2 (Node Float x) (Node Float y) = Node Vector2 $ Call "vec2" $ Binary (Node Float x) (Node Float y)

ast = var Float "iain"

