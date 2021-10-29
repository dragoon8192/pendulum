module Data.Tuple.Extra (
both, (<@$>), (<@*>)
) where

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

(<@$>) = both

(<@*>) :: (a -> b, a' -> b') -> (a, a') -> (b, b')
(<@*>) (f, f') (a, a') = (f a, f' a')
