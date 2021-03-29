module Runtime where
import SyntaxTree
import Language
import Prelude hiding (length, (.),or,and)

makeUndefinedError funcName ast = error $ "Error! No implementation of " ++ funcName ++ " accepts \'" ++ show (typeof ast) ++ "\'.\n The incorrect type is from " ++ show ast

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
perlin (Node Vector2 x) = Node Float $ Call "perlin" [Node Vector2 x]
perlin x = makeUndefinedError "perlin" x

pow :: SyntaxTree -> SyntaxTree -> SyntaxTree
pow (Node Float x) (Node Float y) = Node Float $ Call "pow" [(Node Float x), (Node Float y)]
pow x y = makeUndefinedError "perlin" x

clamp :: SyntaxTree -> SyntaxTree -> SyntaxTree -> SyntaxTree
clamp (Node Float a) (Node Float b) (Node Float x) = Node Float $ Call "clamp" [(Node Float a), (Node Float b), (Node Float x)]
clamp x y z = makeUndefinedError "clamp" (vector (x,y,z))

(^) = pow

data SwizzleAccessor = Accessor String Type

instance GLiteral SwizzleAccessor where
    represent (Accessor name _) = name
    syntaxNode (Accessor name type') = Node type' $ Id name 

if' x _ a _ b = Node (typeof a) $ Call "cond" [x, a, b]
then' = undefined
else' = undefined

and a b = Node (typeof a) $ Call "logand" [a,b]
or a b = Node (typeof a) $ Call "logor" [a,b]
band a b = Node (typeof a) $ Call "bitand" [a,b]
bor a b = Node (typeof a) $ Call "bitor" [a,b]

(&&) = and
(||) = or
(&) = band
(\|) = bor

geq a b = Node Bool $ Call "geq" [a,b]
leq a b = Node Bool $ Call "leq" [a,b]
eq a b = Node Bool $ Call "eq" [a,b]

int x = Node Int $ Call "int" [x]
floor x = Node (typeof x) $ Call "floor" [x]

swizzle :: SyntaxTree -> SwizzleAccessor -> SyntaxTree
swizzle lhs (Accessor name type') = Node type' $ Call "swizzle" [lhs, syntaxNode (Accessor name type')]

(.) = swizzle
(\.) = swizzle
(.>) = swizzle
(.->) = swizzle

-- accessors
x = Accessor "x" Float
y = Accessor "y" Float
xy = Accessor "xy" Vector2
yx = Accessor "yx" Vector2
