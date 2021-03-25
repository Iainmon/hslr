{--
    Language Representation
--}

{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Language where

import SyntaxTree
import CodeGeneration
import RoseTree

class GLiteral t where
    represent :: t -> String
    syntaxNode :: t -> SyntaxTree

instance GLiteral Int where
    represent = (++".0") . show
    syntaxNode n = Node Float $ Id $ represent n
instance GLiteral Integer where
    represent = (++".0") . show
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
    syntaxNode (a,b) = Node Vector2 $ Call "vec2" $ map syntaxNode [a,b]
instance (GLiteral a, b ~ a, c ~ a) => GLiteral (a,b,c) where
    represent (a,b,c) = show $ syntaxNode (a,b,c)
    syntaxNode (a,b,c) = Node Vector3 $ Call "vec3" $ map syntaxNode [a,b,c]


precidence typeA typeB = if typeA > typeB then typeA else typeB
instance Num SyntaxTree where
    -- (*) :: SyntaxTree -> SyntaxTree -> SyntaxTree
    (*) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "mul" [Node lhsType lhs, Node rhsType rhs]
    -- (+) :: SyntaxTree -> SyntaxTree -> SyntaxTree
    (+) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "add" [Node lhsType lhs, Node rhsType rhs]
    (-) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "sub" [Node lhsType lhs, Node rhsType rhs]
    abs (Node type' node) = Node type' $ Call "abs" [ Node type' node ]
    signum = id
    fromInteger n = syntaxNode n

instance Fractional SyntaxTree where
    (/) (Node lhsType lhs) (Node rhsType rhs) = Node (precidence lhsType rhsType) $ Call "div" [Node lhsType lhs, Node rhsType rhs]
    fromRational a = Node Float $ Id $ show $ realToFrac a

wrapPureAST :: SyntaxTree -> SyntaxTree
wrapPureAST (Imparitive instructions ast) = Imparitive instructions ast
wrapPureAST ast = Imparitive [] ast


appendInstructions :: SyntaxTree -> [SyntaxTree] -> SyntaxTree
appendInstructions (Imparitive instructions ast) newInstructions = Imparitive (instructions ++ newInstructions) ast
appendInstructions ast newInstructions = flip appendInstructions newInstructions $ wrapPureAST ast

appendInstruction ast newInstruction = appendInstructions ast [newInstruction]



wrap :: (GLiteral a) => (SyntaxTree -> SyntaxTree) -> a -> SyntaxTree
wrap f = f . syntaxNode

vector :: (GLiteral a) => a -> SyntaxTree
vector = syntaxNode

var :: Type -> String -> SyntaxTree
var ty name = Node ty $ Id name


iden :: String -> AST
iden s = Id s

lit :: (GLiteral a) => a -> SyntaxTree
lit = syntaxNode


data Identifier = Identifier Type String deriving (Eq,Ord,Show)

instance Magnitude Identifier where
    magnitude (Identifier _ _) = 1

toRoseTree :: SyntaxTree -> RoseTree Identifier
toRoseTree (Node type' (Call name args)) = Branch (Identifier type' name) (map toRoseTree args)
toRoseTree (Node type' (Id name)) = Leaf $ Identifier type' name
toRoseTree (Imparitive assignments ast) = toRoseTree ast
toRoseTree (Assignment type' name ast) = toRoseTree ast
