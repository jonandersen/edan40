> import List

4.

> c a = (a \\) . (a \\)

(\\) returns all elements in first list that's not present in the second.
The function c returns the common elements of two lists.


5.

> f = ((/) . (5+))
> g = id


6.

> data Tree = Node [Tree] Int 
>             | Leave Int
>             deriving Show

> a = Node [Node [Leave 4] 2, Node [Leave 5] 3] 1
> b = Node [Leave 5] 3

> subTree :: Tree -> Tree -> Bool
> subTree t1 t2 = True