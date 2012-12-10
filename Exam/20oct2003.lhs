1

> applyPlusOne :: (Num b) => (a -> b) -> a -> b
> applyPlusOne f = (+) 1 . f

2
We can only replicate values of x which are of the enum class. 

> rep n x = take n [x,x..]


3.

> h :: (Eq a) => (a -> a) -> a -> a
> h f = fst . head . dropWhile (uncurry (/=)) . ps (iterate f)
> 	where
> 	ps g x = zip (tail (g x)) (g x)	

h finds the first value when the function f generates the same output as the input, by applying 
its output as input. 

4.

foldr has switched arguments with foldl.

> fLookup :: (Eq a) => a -> [a] -> Maybe a
> fLookup key = foldr (f key) Nothing  
> f :: (Eq a) => a -> a -> Maybe a -> Maybe a
> f key k i 
> 	| key == k = Just k
> 	| otherwise = i

> f1Lookup :: (Eq a) => a -> [a] -> Maybe a
> f1Lookup key = foldl (f1 key) Nothing  

> f1 :: (Eq a) => a -> Maybe a -> a -> Maybe a
> f1 key i k 
> 	| key == k = Just k
> 	| otherwise = i


