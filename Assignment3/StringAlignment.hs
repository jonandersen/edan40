type AlignmentType = (String,String)

-- This function calculutes how similar two strings are
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = scoreSpace * length ys
similarityScore xs [] = scoreSpace * length xs
similarityScore (x:xs) (y:ys) = maximum [score x y +  similarityScore xs ys,
                                         score x '-' +  similarityScore xs (y:ys),
                                         score '-' y + similarityScore (x:xs) ys]
                                         
-- This function uses a dynamic programming approach.                                         
similarityScore2 :: String -> String -> Int
similarityScore2 xs ys = simScore (length xs) (length ys)
  where
    simScore :: Int -> Int -> Int
    simScore i j = simTable!!i!!j
    simTable = [[simEntry i j | j  <- [0..]] | i <-[0..]]
    
    simEntry :: Int -> Int -> Int
    simEntry 0 0 = 0
    simEntry i 0 = scoreSpace * i
    simEntry 0 j = scoreSpace * j
    simEntry i j =  maximum [score x y   + simScore (i-1) (j-1),
                             score x '-' + simScore (i-1) j,
                             score '-' y + simScore i (j-1)]
      where 
        x = xs!!(i-1)
        y = ys!!(j-1)                                             

-- Simple function that only calculates the score of two chars.
score :: Char -> Char -> Int
score x '-' =  scoreSpace
score '-' y = scoreSpace
score x y 
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

-- AttachHeads takes to elements and adds (attaches) them
-- as head in the list. 
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- Attaches the element to the tail 
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTails t1 t2 aList = [(xs ++ [t1],ys ++ [t2]) | (xs,ys) <- aList]

-- This function filters the strings with the best score
-- based of the function provided as a argument
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = fst $ unzip $ filter ((>= maxima).snd) $ zip xs $ values
  where maxima = maximum $ values 
        values = map valueFcn xs  

-- Returns the optimal alignments for the two strings       
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] []     = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs [] 
optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) (y:ys) = maximaBy tupleScore $ concat [attachHeads x y $ optAlignments xs ys , 
                                                            attachHeads '-' y $ optAlignments (x:xs) ys , 
                                                            attachHeads x '-' $ optAlignments xs (y:ys) ] 

-- TupleScore calculates the score of the two strings, how similar they are
tupleScore :: AlignmentType -> Int                     
tupleScore (xs,ys)  = sum $ zipWith score xs ys   

-- Function to calculate and output the matching of two strings
outputOptAlignments xs ys = display $ optAlignments xs ys
outputOptAlignments2 xs ys = display $ optAlignments2 xs ys

-- Function to display a list of matchings.
display :: [AlignmentType] -> IO ()
display xs = do
  putStrLn ("There are " ++ (show (length xs)) ++ " " ++ "optimal alignments:")
  mapM_ (putStr . formatTuple) xs

-- Formats the tuple to appropriate output
formatTuple :: AlignmentType -> String
formatTuple (xs,ys) = "\n" ++ format xs ++ "\n" ++ format ys ++ "\n" 

-- Formats a string to appropriate output
format :: String -> String
format [] = []
format (x:xs) =  x : ' ' : format(xs) 

-- This is a helper function which makes a list of tuples int a tuple
-- with the score (as it should be) the same and the AlignemtTypes as a list
concatTupleList :: [(Int, [AlignmentType])] -> (Int, [AlignmentType])
concatTupleList list = ((fst $ head list), concatMap snd list)

-- Dynamic Programming approach to the string alignment task
optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = snd $  optAlign (length xs) (length ys)
  where
    optAlign :: Int -> Int ->  (Int, [AlignmentType])
    optAlign i j = optTable!!i!!j
    optTable = [[optEntry i j | j  <- [0..]] | i <-[0..]]
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0,[([],[])])
    optEntry i 0 = (scoreSpace + q, (attachTails (xs!!(i-1)) '-' w))
      where (q,w) = optAlign (i-1) 0
    optEntry 0 j = (scoreSpace + q, (attachTails '-' (ys!!(j-1)) w))
      where (q,w) = optAlign 0 (j-1)
    optEntry i j
      | x == y   = (scoreMatch + (fst $ optAlign (i-1) (j-1)), attachTails x y $ snd $ optAlign (i-1) (j-1))
      | otherwise = concatTupleList $ maximaBy fst $ als
        where
          x = xs!!(i-1)
          y = ys!!(j-1)
          als =  [(a + scoreMismatch, attachTails x y b), (c + scoreMismatch, attachTails '-' y d), (e + scoreMismatch, attachTails  x  '-' f)]
            where 
              (a,b) =  optAlign (i-1) (j-1)
              (c,d) =  optAlign  i    (j-1)
              (e,f) =  optAlign (i-1)  j



             
             
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"             
                          


-- TEST -- 
testScore1 = similarityScore string1 string2
assertTestScore1 = testScore1 == -5
testMaximaBy = maximaBy length ["cs", "efd", "lth", "it"] 
assertTestMaximaBy = testMaximaBy == ["efd", "lth"]