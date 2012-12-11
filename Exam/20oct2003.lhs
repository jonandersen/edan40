> import Maybe

1

> applyPlusOne :: (Num b) => (a -> b) -> a -> b
> applyPlusOne f = (+) 1 . f

alternativt:

> applyPlusOne2 f = (\x -> f (x +1))

2
We can only replicate values of x which are of the enum class. 

> rep n x = take n [x,x..]


3.

> h :: (Eq a) => (a -> a) -> a -> a
> h f = fst . head . dropWhile (uncurry (/=)) . ps (iterate f)
> 	where
> 	ps g x = zip (tail (g x)) (g x)	

h finds the first value when the function f generates the same output as the input when the list is shifted one step in one of the alternatives, by applying 
its output as input. 

4.

foldr has switched arguments with foldl.

> fLookup :: (Eq a) => a -> [a] -> Maybe a
> fLookup key = foldr (f key) Nothing  
> f :: (Eq a) => a -> a -> Maybe a -> Maybe a
> f key k i 
> 	| key == k = Just k
> 	| otherwise = i

> f1Lookup :: (Eq a) => a -> [a] -> Maybe a
> f1Lookup key = foldl (f1 key) Nothing  

> f1 :: (Eq a) => a -> Maybe a -> a -> Maybe a
> f1 key i k 
> 	| key == k = Just k
> 	| otherwise = i


5.

> a f = flip (curry f)
> b f = curry (f.swap)
> swap :: (a,b) -> (b,a)
> swap (a, b) =  (b, a)

6.

> x = Just 5
> y = Just 3
> z = Nothing


We can use applicative functors. This is not included in the version we currently run.
This is how it would be done.

>> pure (+) <*> x <*> y
>> pure (+) <*> x <*> z
>> pure (+) <*> (pure (\x -> (x+x)) <*> x) <*> (pure (\x -> (x+x)) <*> y)



För att hantera additioner av typen Just måste man först konvertera x och y
fron typen maybe till en typ som det går att utföra aritmetiska utryck på.
Detta gör man med functionen fromJust. Därefter kan man utföra additionen och
konvertera resultatet till Just igen. 
Om man stöter på en Nothing så betyder det att någonting har inte gått som det
skulle och detta måste hanteras. Man kan mha funktionen isJust kontrollera om 
man har en Nothing eller en Just och därefter definiera vad man vill göra för 
de olika fallen.