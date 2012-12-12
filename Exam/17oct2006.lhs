1.

> f :: (Eq a) => [a] -> [a] -> [a]
> f = filter . flip elem

> g ys xs = [x | x <- xs , elem x ys]

2.
Currying is the process of transforming a function that takes several arguments 
into a function that takes only a single argument. With this we can partially apply
function which is used in infix operators. Also it becomes easier to prove your functions.

3.

> es :: Maybe t -> Maybe Bool
> es k = do 
>	x <- k
> 	Nothing
> 	return False

4.

> a :: Integer -> Integer
> a = (8-)
> b :: Integer -> Integer
> b = (+0).(0+)
> c :: (a1 -> b -> c) -> a1 -> (a -> b) -> a -> c
> c = (.)(.)
> d :: a -> [a]
> d = (:[])
> e :: (a -> b) -> a -> b
> e = (($)$($))
> --f = ([]>>=)(\_->[(>=)])

5a. 
m1 has two music elements, one with length dur*2 and one with length dur
m2 has 3 music elements each with length dur

5b.

line2 = lineToList . line

5c.

6.

> map2 a = foldr (\x xs-> f x :xs) []