1
/////LISTCOMPREHENSION

> c :: a -> [a -> b] -> [b]
> c x = map ($x

> cListComp :: a -> [a -> b] -> [b]
> cListComp x ys = [y x | y <- ys]

2
///////ZIP/////////
Zip tar in 2 listor och kombinerar dem til en lista av tuplar där index 0 av lista a matchas med index 0 av lista b.
unZip tar in en lista av tuplar och separerar dem till två listor.

3
///////PATTERN MATCHING//////////

> oneOf :: Bool -> Bool -> Bool -> Bool
> oneOf False False c = c
> oneOf False b False = b
> oneOf a False False = a
> oneOf _ _ _ = False


4
//////////DO NOTATION///////////

> g :: [Char]
> g = do [1,2,3]; "lambda"

do tar in en lista med längden 3 och kommer därför att göra det som står efter ; 3 gånger och därmed returnera "lambdalambdalambda"

5
//////////CLASSES
I haskell representerar klasser functioner som går att göra på en viss data structur. Använder en datastruktur sig av en klass Eq så kan man jämföra element i data strukturen om de är lika eller ej.

Hade inte klasser funnits i haskell hade man själv behövt definiera vad som menas med att tex visa, jämföra läsa och vad en Integral är för något. 


6
\\\\\\\\\\\TODO\\\\\\\\\\\\\
c a = (a\\) . (a\\)
(\\) = foldl (flip delete)
delete x [] = []
delete x (y:ys) = if x==y then ys else y:delete x ys