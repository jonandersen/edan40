-- 1
-- The function f takes two arguments. The first argument is a type which is possible to order. The second 
-- is a list of numbers. The function produces a list of tuples were the first argument and the second
-- is created as a tuple. 

-- 2
adjustLengthTo xs ys = snd $ unzip $ zip xs ys

-- 3
e::[Int]
e = do
  x <- return (5 + 2) -- x = 7
  return 4 -- e = 4
  return (7*x) -- e = 7 * 7 = 49

-- 4 

-- 5 
-- Currying is the process of transforming a function that takes several arguments 
-- into a function that takes only a single argument. With this we can partially apply
-- function which is used in infix operators. Also it becomes easier to prove your functions.

f :: a -> b -> c  -- uncurried
g :: (a,b) -> c -- curried

f = curry g
g = uncurry f


-- 6
