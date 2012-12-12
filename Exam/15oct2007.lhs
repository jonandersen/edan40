1.

> f = (+) 1 . (*) 3
> fs = (+) . (*) 3

2.

> function f xs ys = map (uncurry f) (zip xs (map (*3) (filter (>0) ys)))

3.

Haskell classes can be seen as Java interface. A data structure of a certain 
class tells you what you can do with an element of that data structure. For 
instance an element of class Eq a can be compared for equality and a element
of class Ord can be ordered.

4.

data Bush a = Leaves [a] | Trunk (Bush [a])

1. Leaves [1] [2] [3]
Leaves takes only one list as an argument therfore this is not valid
2. Leaves [1,2,3]
This is typcorrect acording to above stated reason
3. Trunk (Leaves [[1,2], [6,7]])
Trunk takes a bush and a bush has a list of elements a which in this case 
a = [Int]
4. Trunk (Trunk (Leaves [[[5],[6],[[8],[9]]],[[6,19],[20]]])) 
This doesent work because not al elemnts are of the type a = [[[Int]]] but som 
are of [[Int]]
5. Trunk (Trunk (Leaves [[[5],[6,8,9],[]],[[6,19],[20]]]))
Here everything is fine because the lists are of the same type [[a]]

5.

> s::(Num a, MonadPlus m) => m a
> 	s = do
> 	x <- return 3
> 	mzero
> 	y <- return 5
> 	return y

6.