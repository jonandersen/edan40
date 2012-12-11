> import List

1.

> mapper :: [a] -> [([a] -> [a] -> [a])]
> mapper = map (const (++)) 

2.
Lazy evaluation means that the value is not calculated until it is actually needed.


3. 


> fibonacci = unfoldr (\[a,b] -> Just(a+b,[b,b+a])) [0,1]

:t unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
:t foldr :: (a -> b -> b) -> b -> [a] -> b

> a xs z = unfoldr f' (foldr f z xs)

If the following holds:

f' (f x y) = Just (x,y)
f' z       = Nothing




