1.

> test :: Bool -> Bool -> Bool -> Bool
> test True True c = not c
> test a True True = not a
> test True b True = not b
> test _ _ _ = False

2.

> g f = (\x -> f ((f x)/3))
> g1 f = f . (/) 3 . f

3.

> e :: (Monad m, Num a) => m a -> m Bool
> e k = do
> 	x <- k
> 	return (2*x)
> 	return False

> e1 k = k >>= (\x -> return (2*x)) >> return False

4.

5.
As this is a rather broad question it's hard to give a brief answer. 
Monads is a way to wrap things and provide methods to do operations on the wrapped things without unwrapping. 

It can be called "computation builder". 

Monads is used to handle side effects. 

> data W a = W a deriving Show

> return1 :: a -> W a
> return1 x = W x

> fmap1 :: (a -> b) -> (W a -> W b)
> fmap1 f (W x) = W (f x)

> bind1 :: (a -> W b) -> (W a -> W b)
> bind1 f (W x) = f x

> f :: Int -> W Int
> f x = W (x+1)

> a = bind1 f (bind1 f (f 1))

Or a as a Monad

> instance Functor W where
>    fmap f (W x) = W (f x)

> instance Monad W where
>    return x = W x
>    W x >>= f = f x

> a1 = (W 1) >>= return.(+1)  >>= return.(+1)


6.

> unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
> unfoldr f b = case f b of 
> 	Nothing -> []
> 	Just (a,b) -> a : unfoldr f b


> fibonacci = unfoldr (\[a,b] -> Just(a+b,[b,b+a])) [0,1]

:t unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
:t foldr :: (a -> b -> b) -> b -> [a] -> b

> a xs z = unfoldr f' (foldr f z xs)

If the following holds:

f' (f x y) = Just (x,y)
f' z       = Nothing







