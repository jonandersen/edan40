1.

> f :: (Eq a) => [a] -> [a] -> [a]
> f = filter . flip elem

This is evaluted as ((\x -> filter (flip elem x))y)

> a :: (Eq a) => [a] -> a -> Bool
> a = flip elem

> f1 :: (Eq a) => [a] -> [a] -> [a]
> f1 xs ys = [x | x <- xs, elem x ys]

2.
Currying is the process of transforming a function that takes several arguments 
into a function that takes only a single argument. With this we can partially 
apply function which is used in infix operators. Also it becomes easier to prove
your functions.

3.

> es :: Maybe t -> Maybe Bool
> es k = do 
>  x <- k
>  Nothing
>  return False

> e k = (return k) >>= (\x -> return x) >> Nothing >> return False

4.

> a1 :: (Num a) => (a -> a)
> a1 = (8-)
> a2 :: (Num a) => (a -> a)
> a2 = (+0).(0+)
> a3 :: (a1 -> b -> c) -> a1 -> (a -> b) -> a -> c
> a3 = (.)(.)
> a4 :: a -> [a]
> a4 = (:[])
> a5 :: (a -> b) -> a -> b
> a5 = (($)($)($))

6.

> map2 a = foldr (\x xs-> f x :xs) []
