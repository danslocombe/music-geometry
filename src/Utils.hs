module Utils where

firstBelow :: Ord a => [a] -> a -> Maybe a
firstBelow xs y = firstBelow' xs y Nothing

firstBelow' :: Ord a => [a] -> a -> Maybe a -> Maybe a
firstBelow' [] _ z = z
firstBelow' (x:xs) y z = if x >= y then z else firstBelow' xs y (Just x)

firstAbove :: Ord a => [a] -> a -> Maybe a
firstAbove [] _ = Nothing
firstAbove (x:xs) y = if x >= y then Just x else firstAbove xs y
