module Runtime where
import SyntaxTree
import Language
import Prelude hiding (length, (.))

makeUndefinedError funcName ast = error $ "Error! No implementation of " ++ funcName ++ " accepts " ++ show ast

cosine :: Function
cosine (Node Float x) = Node Float $ Call "cos" [Node Float x]
cosine (Node Vector2 x) = Node Vector2 $ Call "cos" [Node Vector2 x]
cosine (Node Vector3 x) = Node Vector3 $ Call "cos" [Node Vector3 x]
cosine x = error $ "No definition for " ++ show x

sine :: Function
sine (Node Float x) = Node Float $ Call "sin" [Node Float x]
sine (Node Vector2 x) = Node Vector2 $ Call "sin" [Node Vector2 x]
sine (Node Vector3 x) = Node Vector3 $ Call "sin" [Node Vector3 x]

root :: Function
root (Node Float x) = Node Float $ Call "sqrt" [Node Float x]
root x = makeUndefinedError "root" x
sqrt = root

-- makeUnaryFunction :: String -> Type -> Type -> SyntaxTree -> SyntaxTree
-- makeUnaryFunction name inType outType = f
--     where f (Node inType x) = Node outType $ Call name $ Node inType x
-- makeGenericUnaryFunction :: String -> SyntaxTree -> SyntaxTree
-- makeEndomorphicUnaryFunction name = f
--     where f (Node dType x) = Node dType $ Call name $ Node dType x

makeFunctionApplication :: String -> Type -> Type -> Function
-- makeFunctionApplication name inType outType (Node inType' (Call name' (Binary args))) = Node outType $ Call name $ Binary $ Node inType' $ Call name' $ Binary args
-- makeFunctionApplication name inType outType (Node inType' (Call name' (Trinary args))) = Node outType $ Call name $ Trinary $ Node inType' $ Call name' $ Trinary args
makeFunctionApplication name inType outType (Node inType' x) = Node outType $ Call name [Node inType' x]

length :: Function
length (Node type' x) = makeFunctionApplication "length" type' Float $ Node type' x

normalize :: Function
normalize (Node type' x) = makeFunctionApplication "normalize" type' Float $ Node type' x

smoothstep :: SyntaxTree -> SyntaxTree -> SyntaxTree -> SyntaxTree
smoothstep (Node Float a) (Node Float b) (Node Float x) = Node Float $ Call "smoothstep" [(Node Float a), (Node Float b), (Node Float x)]

mix :: SyntaxTree -> SyntaxTree -> SyntaxTree -> SyntaxTree
mix (Node type' a) (Node type'' b) (Node Float x) = Node (if type' == type'' then type' else Void) $ Call "mix" [(Node type' a), (Node type'' b), (Node Float x)]

abs :: Function
abs (Node type' x) = makeFunctionApplication "abs" type' type' $ Node type' x

perlin :: Function
perlin (Node Vector2 x) = Node Float $ Call "perlin" [Node Vector3 x]
data SwizzleAccessor = Accessor String Type

instance GLiteral SwizzleAccessor where
    represent (Accessor name _) = name
    syntaxNode (Accessor name type') = Node type' $ Id name 


swizzle :: SyntaxTree -> SwizzleAccessor -> SyntaxTree
swizzle lhs (Accessor name type') = Node type' $ Call "swizzle" [lhs, syntaxNode (Accessor name type')]

(.) = swizzle

-- accessors
x = Accessor "x" Float
y = Accessor "y" Float
xy = Accessor "xy" Vector2
yx = Accessor "yx" Vector2
