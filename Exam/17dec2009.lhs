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

zipwith map takes 2 lists a and b and maps whats in a with whats in b

map zipwith applies zipwoth on every element of a list

map . zipwith maps the result of zipwith f x y with a list z.

6.
One folds from right and one from left.

> mapper f = foldr (\x xs -> (f x) : xs) []