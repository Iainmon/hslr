{--
    Language Representation
--}

{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Language where

import SyntaxTree
import CodeGeneration

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



wrap :: (GLiteral a) => (SyntaxTree -> SyntaxTree) -> a -> SyntaxTree
wrap f = f . syntaxNode
