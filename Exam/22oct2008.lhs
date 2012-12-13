> import List

1.

> a = map (:)

a applied to a list of elements generates a list of partially applied functions 
which has an element and require a list. 

2.

> b :: [[Char]] 
> b = do "hello"; return "world"

returnerar ["world","world","world","world","world"]

3.

A function in functional programming given the value x will always provide the 
value y and never anything else. This is good since you can prove that your 
programs work is intended. 

4.

> test (a,b) = a*b+1
> test2 (a,b) = a*b+2 
> s :: [(a,b) -> c] -> [a -> b -> c]
> s xs = map curry xs

5.

> g :: (Eq a) => [a] -> [[a]]
> g [] = [[]]
> g xs = concat [ map (x:) (g (xs \\ [x])) | x <- xs]

This function creatse all permutations of the list elements. 
The empty list is needed as we make a recursive call and we need an stop case.

6.

> fil p = foldr (\x xs -> if p x then (x:xs) else xs) []