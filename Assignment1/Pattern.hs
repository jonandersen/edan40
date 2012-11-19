module Pattern where
import Utilities
import Maybe

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ xs [] = xs 
substitute _ [] _ = []
substitute wc (x:xs) ys 
  | wc == x    = ys ++ (substitute wc xs ys)
  | otherwise = x:(substitute wc xs ys)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (x:xs) 
  | x == p    = match wc ps xs
  | wc == p   = singleWildcardMatch (p:ps) (x:xs) `orElse` longerWildcardMatch (p:ps) (x:xs)
  | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) 
  | isJust $ match wc ps xs = Just [x]
  | otherwise = Nothing

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

testPattern2 = "do you know what * is"
testSubstitutions2 =  "what is *"
testString2 =  "do you know what Haskell is"
substituteTest2 = substitute '*' testPattern2 testSubstitutions2

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

listTransform = [("Large * ice cream", "Un grande glace a'la *"),("My name is *", "Je m'appelle *"),("Cats name is *", "Je m'appelle *")]
listTransform2 = [("Large * ice cream", "Un grande glace a'la *"),("My name is *", "Je m'appelle *")]
testTransform = transformationsApply '*' id listTransform "My name is Zacharias"
testTransform2 = transformationsApply '*' id listTransform "Large vanilla ice cream"
testTransform3 = transformationsApply '*' id listTransform "Small vanilla ice cream"

listTransform5 = [( "please *", "*" )]
testTransform5 = transformationsApply '*' id listTransform5 "can you please tell me what Haskell is"

listTransform3 = [("","Speak up! I can't hear you."),("I need *","Why do you need * ?")]
testTransform4 = transformationsApply '*' id listTransform3 "I need a cat"

listTransform4 = [("I hate *",  "Why do you hate * ?")] 
testTransform6 = transformationsApply '*' id listTransform4 "ARGH!"


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f xs t = mmap (substitute wc (snd t)) $ mmap f (match wc (fst t) xs)


--  foldl (transformationApply wc f xs) ts

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (t:ts) xs   
  | isJust $ transformationApply wc f xs t  = transformationApply wc f xs t
  | otherwise = transformationsApply wc f ts xs

