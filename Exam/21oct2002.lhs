1
/////LISTCOMPREHENSION

> c :: a -> [(a -> b)] -> [b]
> c x = map ($x)

> cListComp :: a -> [(a -> b)] -> [b]
> cListComp x fs = [f x | f <- fs]

2
///////ZIP/////////
Zip tar in 2 listor och kombinerar dem til en lista av tuplar där index 0 av lista a matchas med index 0 av lista b.
unZip tar in en lista av tuplar och separerar dem till två listor.

Both zip and unzip works on (or with) tuples. 
Zip creates a list of tuples where each tuple consist of two elements from two lists. The two elements match 
on index. 
Unzip creates a tuple of two lists from a tuple list. The first tuple element is a list of elements
all taken from the first tuple element in the tuple list. The second tuple element is taken from the second tuple element in the tuple list. 


3
///////PATTERN MATCHING//////////

> oneOf :: Bool -> Bool -> Bool -> Bool
> oneOf False False c = c
> oneOf False b False = b
> oneOf a False False = a
> oneOf _ _ _ = False


4
//////////DO NOTATION///////////

extract from implementation of list monad
xs >>= f = concat (map f xs)

> m = "123" >>= (\x -> (x:"lambda"))

or 

> m1 = [1,2,3] >>= (\_ -> ("lambda"))

It's a list monad. 

> g :: [Char]
> g = do 
> 	x <- [1,2,3]
> 	"lambda"

do tar in en lista med längden 3 och kommer därför att göra det som står efter 
; 3 gånger och därmed returnera "lambdalambdalambda"

5
//////////CLASSES
I haskell representerar klasser funktioner som går att göra på en viss data 
struktur. Använder en datastruktur sig av en klass Eq så kan man jämföra element
 i data strukturen om de är lika eller ej.

Hade inte klasser funnits i haskell hade man själv behövt definiera vad som
menas med att tex visa, jämföra läsa och vad en Integral är för något. 


6
\\\\\\\\\\\TODO\\\\\\\\\\\\\
c a = (a\\) . (a\\)



(\\) = foldl (flip delete)

delete deletes the first occurrence of x.  

> delete x [] = []
> delete x (y:ys) = if x==y then ys else y:delete x ys
> c1 a = (a (foldl (flip delete)))



