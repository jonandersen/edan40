\section{Haskore CSound Tutorial}
\label{csound-tut}

EDAN40 - Functional Music
Jon Andersen ada08jan@student.lu.se 
Marcus Carlberg ada08mca@student.lu.se

Introduction
AutomComp is a program that given a list of Chords and a Key of a song can generate 
a 3 different types of bass lines and chords to a song. 

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 
> import Maybe

///UTIL STUFF///

To make everything work different defenitions had to be made for the different 


> type Key = (PitchClass, Mode)
> type Note = (PitchClass, Octave)
> type NoteList = [Note]
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]
> type Triad = [Int]
> type Scale = [Int]


> cmaj = (C, Major)
> fmaj = (F, Major)
> gmaj = (G, Major)


> fd d n = n d v
> vol  n = n   v
> v      = [Volume 100]
> lmap f l = line (map f l)

> times  1    m = m
> times (n+1) m = m :+: (times n m)

Dessa tva borde vi kunna gora battre, eller hitta en fardig funktion som gor detta?

> lookupNote :: [(PitchClass, Int)] -> Int -> Maybe PitchClass
> lookupNote [] _ = Nothing
> lookupNote ((t1,t2):ts) m
>		| m == t2 = Just t1
>		| otherwise = lookupNote ts m


> lookupInt :: [(PitchClass, Int)] -> PitchClass -> Maybe Int
> lookupInt [] _ = Nothing
> lookupInt ((t1,t2):ts) m
>		| m == t1 = Just t2
>		| otherwise = lookupInt ts m


> notes = zip (cycle [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]) [0,1..]

Our model for chords, can be expanded

///BASS///

> type BassStyle = [(Int, Dur)]

> basic, calypso, boogie :: BassStyle
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]


> autoBass :: BassStyle -> Key -> [(PitchClass, Dur)] -> Music
> autoBass [] _ _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass _ _ [] = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass (b:bl) k (c:cl) = foldr1 (:=:) (handleRest c b (getScale k c)) :+: autoBass bl k cl

> getScale :: Key -> Chord ->[Int]
> getScale k c
> 	| snd k == Major = ionian
>  	| otherwise = isDorian k c
> 	where
> 	isDorian :: Key -> Chord -> [Int]
> 	isDorian k c
> 		| (fromJust $ lookupInt notes (fst c)) == ((fromJust $ lookupInt notes (fst k)) + 3) = dorian
> 		| otherwise = aeolian

> handleRest :: Chord -> (Int, Dur) -> Scale -> [Music]
> handleRest c1 b1 sc1
> 	|fst b1 == -1 = [Rest (snd b1)]
> 	|otherwise = [Note (fst note, pitch ) (snd b1) [Volume 65]]
> 	where
> 	note = (!!) notes  (mod (((!!) sc1 (fst b1)) + (fromJust $ lookupInt notes (fst c1))) 12)
> 	pitch = 2 + div (fromJust $ lookupInt notes (fst c1)) 12


> splitChord :: BassStyle -> [(PitchClass, Dur)] -> [(PitchClass, Dur)] 
> splitChord [] _ = []
> splitChord _ [] = []
> splitChord (b:bl) (c:cl)
> 	| snd c > snd b = splitChord (b:bl) ([(fst c, (snd c)/2), (fst c, (snd c)/2)] ++ cl)
> 	| otherwise =   c: splitChord bl cl

///CHORDS///

> findTriad :: Key -> PitchClass -> Triad
> findTriad (key, mode) note
> 	| mode == Major = [0,4,7]
> 	| otherwise = [0,3,7]								
																					
If C, [0,4,7] -> [C,E,G]

> findPitch rootNote position = fromJust $ lookupNote notes (mod ( (fromJust $ lookupInt notes rootNote) + position) 12)

> findPitchInt pitch = ((fromJust $ lookupInt notes (fst pitch)) + (snd pitch) * 12)

> noteList = take 16 $ drop 52 notes

> createFirstChord :: PitchClass -> Triad -> NoteList			
> createFirstChord _ [] = []
> createFirstChord n (p:ps) = (findPitch n p, div noteInt 12) :  createFirstChord n ps
> 	where noteInt = fromJust $ lookupInt noteList $ findPitch n p

> createChord :: NoteList -> PitchClass -> Triad -> NoteList															
> createChord _ _ [] = []											
> createChord (prev:prevs) n (p:ps) = (pitch, div noteInt 12) :  createChord prevs n ps
>    where noteInt = findClosets (findPitchInt prev) pitch
>          pitch = findPitch n p

> findClosets :: Int -> PitchClass -> Int
> findClosets n p  
>    | abs(n - normal) <= abs(n - reversed) = normal
>    | otherwise     												= reversed 
>    where normal = (fromJust $ lookupInt noteList p)
>          reversed = (fromJust $ lookupInt (reverse noteList) p)  

> sumOfChord :: NoteList -> Int
> sumOfChord chords =  abs $ foldl1 (-) $ zipWith (*) (snd $ unzip chords) (map (fromJust) $ map (lookupInt notes ) $ fst $ unzip chords)


> findTightest :: NoteList -> NoteList
> findTightest [] = []
> findTightest (x:xs) 
>    | summed  > summedNew = x:findTightest xs
>    | otherwise = x:findTightest xs
>    where summed = sumOfChord (x:xs)
>          summedNew = sumOfChord (x:xs) 


> testFindClosets = findClosets 55 G 

> testCreateChord = createChord [(C,5),(E,4),(G,4)] G [0,4,7] 
> testCreateChord2 = createChord [(F,4),(A,4),(C,5)] F [0,4,7] 
> ensureCreateChord = [(G,4),(B,4),(D,4)]

testChords :: NoteList
testChords = [(C,4),(D,4)(E,4)]

[(E,4),(G,4),(C,5)]

[(E,4),(C,5),(G,5)]

 testSumOfChord = sumOfChord 


This maps some notes to a chord. 

> mapChord :: NoteList -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note x dur [Volume 60] | x <- chord ]

///AUTOMUSIC///



stairwayChords = [(A, Minor), hn]

AutoBass creates the bass line of the song.
autoBass :: BassStyle -> Key -> ChordProgression -> Music

AutoChord generates the chords of the song.

																					Needs to be updated

> createChords :: Key -> ChordProgression -> NoteList -> [Music] 
> createChords _ [] _ = []																
> createChords rootKey ((note,dur):keys) previous = (mapChord current dur) : createChords rootKey keys current
>    where current = findTightest (createChord previous note $ findTriad rootKey note)																
																					

> autoChord :: Key -> ChordProgression -> [Music] 
> autoChord rootKey cp = createChords rootKey cp headChord
>    where headChord = (createFirstChord (fst $head cp) $ findTriad rootKey (fst $ head cp))


autoComp creates a song with a baseline and chords.


> autoComp :: BassStyle -> ChordProgression -> Key -> Music
> autoComp bs cp key = (Instr "piano" $ Tempo 2 $ (foldr1 (:+:) (autoChord key cp))) :=: (Tempo 2 $ autoBass bs key $ splitChord bs cp)


MIGHT COME IN HANDY




> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

