> import List

1.

> mapper :: [a] -> [([a] -> [a] -> [a])]
> mapper = map (const (++)) 

2.
Lazy evaluation means that the value is not calculated until it is actually needed. This means that haskell can handle infinite data structures.


3. 

> g :: (a->a) -> a -> Maybe (a,a)
> g f x = Just (x, f x)
> iterate1 :: (a->a) -> a -> [a]
> iterate1 = unfoldr . g

5.

> fmapper f m = do
>   x <- m
>   return (f x)





6a. 
This function removes duplicates in the list.
q :: (Eq a) => [a] -> [a]

> q [] = []
> q (x:xs) = x : q (filter (/=x) xs)

6.
using nub, doesn't seem to be 'the way'.

> l :: (Eq a) => [a] -> [a]
> l = foldr f []

> f :: (Eq a) => a -> [a] -> [a]
> f x xs
>   | elem x xs = xs
>   | otherwise = x:xs

