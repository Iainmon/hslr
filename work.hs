{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}

data OneTuple a = OneTuple a deriving Show
data Identity x = Identity x deriving (Eq)

get :: Identity x -> x
get (Identity x) = x

class MyClass a where
    prints :: a -> String

instance (Show n) => MyClass (n, n) where
    prints (a,b) = show a

instance (Show n) => MyClass (n, n, n) where
    prints (a, b, c) = foldl (++) "" $ map (++" ") $ map show [a,b,c]

instance (Show n) => MyClass (OneTuple n) where
    prints (OneTuple a) = show a

-- tuplify :: (MyClass b) => a -> b
-- tuplify (a,b) = (a,b)
-- tuplify (a,b,c) = (a,b,c)
-- tuplify a = OneTuple a



wrap :: (MyClass a) => (String -> String) -> a -> String
wrap f = f . prints

stringFunction :: String -> String
stringFunction = id



instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

class Tupify a where
    type MyClassType a
    tupify :: a -> MyClassType a


-- instance (Show a) => Tupify (a,a) (a,a) where
--     tupify = id
-- instance (Show a) => Tupify (a,a,a) (a,a,a) where
--     tupify = id
instance (Show a) => Tupify (Identity (a,a)) where
    type MyClassType (Identity (a,a)) = (a,a)
    tupify = get

instance (Show a) => Tupify (Identity a) where
    type MyClassType (Identity a) = OneTuple a
    tupify (Identity a) = OneTuple a

-- vectorToString :: Tupify a b => a -> String
-- vectorToString x = wrap stringFunction $ tuplify x

-- vectorToString (1,2,3)
-- vectorToString (1,2)
-- vectorToString 1
