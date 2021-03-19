module Runtime where
import SyntaxTree
import Language
import Prelude hiding (length)

makeUndefinedError funcName ast = error $ "Error! No implementation of " ++ funcName ++ " accepts " ++ show ast

cosine :: SyntaxTree -> SyntaxTree
cosine (Node Float x) = Node Float $ Call "cos" $ Unary (Node Float x)
cosine (Node Vector2 x) = Node Vector2 $ Call "cos" $ Unary (Node Vector2 x)
cosine (Node Vector3 x) = Node Vector3 $ Call "cos" $ Unary (Node Vector3 x)
cosine x = error $ "No definition for " ++ show x

sine :: SyntaxTree -> SyntaxTree
sine (Node Float x) = Node Float $ Call "sin" $ Unary (Node Float x)
sine (Node Vector2 x) = Node Vector2 $ Call "sin" $ Unary (Node Vector2 x)
sine (Node Vector3 x) = Node Vector3 $ Call "sin" $ Unary (Node Vector3 x)

root :: SyntaxTree -> SyntaxTree
root (Node Float x) = Node Float $ Call "sqrt" $ Unary (Node Float x)
root x = makeUndefinedError "root" x
sqrt = root

-- makeUnaryFunction :: String -> Type -> Type -> SyntaxTree -> SyntaxTree
-- makeUnaryFunction name inType outType = f
--     where f (Node inType x) = Node outType $ Call name $ Node inType x
-- makeGenericUnaryFunction :: String -> SyntaxTree -> SyntaxTree
-- makeEndomorphicUnaryFunction name = f
--     where f (Node dType x) = Node dType $ Call name $ Node dType x

makeFunctionApplication :: String -> Type -> Type -> SyntaxTree -> SyntaxTree
-- makeFunctionApplication name inType outType (Node inType' (Call name' (Binary args))) = Node outType $ Call name $ Binary $ Node inType' $ Call name' $ Binary args
-- makeFunctionApplication name inType outType (Node inType' (Call name' (Trinary args))) = Node outType $ Call name $ Trinary $ Node inType' $ Call name' $ Trinary args
makeFunctionApplication name inType outType (Node inType' x) = Node outType $ Call name $ Unary $ Node inType' x

length :: SyntaxTree -> SyntaxTree
length (Node type' x) = makeFunctionApplication "length" type' Float $ Node type' x

normalize :: SyntaxTree -> SyntaxTree
normalize (Node type' x) = makeFunctionApplication "normalize" type' Float $ Node type' x
