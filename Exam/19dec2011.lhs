6.

> data Tree = Node [Tree] Int 
>             | Leave Int
>             deriving Show

> a = Node [Node [Leave 4] 2, Node [Leave 5] 3] 1
> b = Node [Leave 5] 3

> subTree :: Tree -> Tree -> Bool
> subTree t1 t2 = True