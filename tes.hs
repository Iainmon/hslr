shove = flip (>>=)

add = (+)
    
addMaybes :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybes a b = b >>= (a >>= (\a b -> return $ add a b))


fmap (+) [1,1] <*> [2]
