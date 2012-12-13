1.

> f1 =  flip (/) 3 . (5-)
> f2 =  (/) . (5-)

2.

> f :: (Num a, Monad m) => m a -> m a -> m a
> f x y= do
>   a <- x
>   b <- y
>   return (a * b)

3. Curried form is when a function only takes one argument and produces one 
result. If a function needs several arguments it's transformed to several
functions with each argument partially applied.
The major advantage is that you can construct formal proof for your functions

4. It works only for enumarted x, since .. is a syntax for this.

5. 

6.
One folds from right and one from left.

> mapper f = foldr (\x xs -> (f x) : xs) []