> import Maybe

1

> applyPlusOne f = (\x -> f (x +1))

2
För att man inte kan garantera att x är av klassen Enum som krävs för att föra
en lista på det sättet.

3
Tar en lista shiftar ett steg och sen slår ihop den med urspringet. Därefter
kollar den om några element matchar i så fall returnerar den det första
elementet som matchar

> h f = fst . head . dropWhile (uncurry (/=)) . ps (iterate f)
>
> ps g x = zip (tail (g x)) (g x)

4

> loookup key []   =  Nothing
> loookup key ((x,y):xys)
> 	|key==x = Just y
> 	|otherwise = loookup key xys

 g key = foldl (f key) Nothing 
 	where
 	f key tuple

5

> a f = flip (curry f)
> b f = curry (f.swap)
> swap :: (a,b) -> (b,a)
> swap (a, b) =  (b, a)

6

För att hantera additioner av typen Just måste man först konvertera x och y
fron typen maybe till en typ som det går att utföra aritmetiska utryck på.
Detta gör man med functionen fromJust. Därefter kan man utföra additionen och
konvertera resultatet till Just igen. 
Om man stöter på en Nothing så betyder det att någonting har inte gått som det
skulle och detta måste hanteras. Man kan mha funktionen isJust kontrollera om 
man har en Nothing eller en Just och därefter definiera vad man vill göra för 
de olika fallen. 

> x = Just 5
> y = Just 3
> z = Nothing