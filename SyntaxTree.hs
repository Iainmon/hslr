-- {-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, RankNTypes #-}

{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}


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

class (Show t) => GLiteral t where
    represent :: t -> String
    syntaxNode :: t -> SyntaxTree

class Vectorable v where
    type Component v
    components :: v -> [Component v]

instance (Floating a) => Vectorable (a,a) where
    type Component (a,a) = a
    components (a,b) = [a,b]

-- type Arithmatic = Int | 

-- instance GLiteral Float where
--     represent = show
--     syntaxNode n = Node Float $ Id $ represent n
-- instance GLiteral Double where
--     represent = show
--     syntaxNode n = Node Float $ Id $ represent n
instance GLiteral Int where
    represent = show
    syntaxNode n = Node Float $ Id $ represent n
    
-- instance (Num t) => GLiteral t where
--     represent = show
--     syntaxNode n = Node Int $ Id $ represent n

-- instance (Show num, Num num) => GLiteral num where
--     represent = (show :: (Num a, Show a) => a -> String)
--     syntaxNode n = Node Int $ Id $ represent n

-- instance GLiteral Bool where
--     represent True = "true"
--     represent False = "false"
--     syntaxNode b = Node Bool $ Id $ show b

-- instance (GLiteral a, GLiteral b) => GLiteral (a,b) where
--     represent = show . syntaxNode
--     syntaxNode (a, b) = Node Vector2 $ Call "vec2" $ Binary (syntaxNode a, syntaxNode b)
    
-- instance (Num a, Num b, GLiteral a, GLiteral b) => GLiteral (a,b) where
--     represent = show . syntaxNode
--     syntaxNode (a,b) = Node Vector2 $ Call "vec2" $ Binary (syntaxNode a,syntaxNode b)
instance (Vectorable v, Component v ~ (a,a)) => GLiteral a where
    represent (a,b) = show $ syntaxNode (a,b)
    syntaxNode (a,b) = Node Vector2 $ Call "vec2" $ Binary (syntaxNode a, syntaxNode b)
    


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

cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine _ = undefined

wrap :: (GLiteral a) => (SyntaxTree -> SyntaxTree) -> a -> SyntaxTree
wrap f = f . syntaxNode

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

