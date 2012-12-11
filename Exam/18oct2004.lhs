1.

> test :: Bool -> Bool -> Bool -> Bool
> test True True c = not c
> test a True True = not a
> test True b True = not b
> test _ _ _ = False

2.

> g f = (\x -> f ((f x)/3))

3.
e :: (Monad m, Num a) => m a -> m Bool

> e k = do
>	x <- k
> 	return (2*x)
> 	return False

4.

5.

6.