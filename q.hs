{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

class MyClass a where
    prints :: a -> String

instance MyClass Integer where prints = show
instance MyClass Int where prints = show
instance MyClass Float where prints = show
instance MyClass Double where prints = show

instance (MyClass a, b~a) => MyClass (a,b) where
    prints (x,y) = '(' : prints x ++ "," ++ prints y ++ ")"

instance (MyClass a, b~a, c~a) => MyClass (a,b,c) where
    prints (x,y,z) = '(' : prints x ++ "," ++ prints y ++ "," ++ prints z ++ ")"

