> import List

1.

> mapper :: [a] -> [([a] -> [a] -> [a])]
> mapper = map (const (++)) 

2.
Lazy evaluation means that the value is not calculated until it is actually needed.


3. 

> g :: (a->a) -> a -> Maybe (a,a)
> g f x = Just (x, f x)
> iterate1 :: (a->a) -> a -> [a]
> iterate1 = unfoldr . g



4.

5.

> fmapper f m = do
>   x <- m
>   return (f x)

6.

> q :: (Eq a) => [a] -> [a]
> q [] = []
> q (x:xs) = x : q (filter (/=x) xs)

This function removes duplicates in the list.

