\section{Haskore CSound Tutorial}
\label{csound-tut}

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 
> import Maybe

///UTIL STUFF///

> type Key = (PitchClass, Mode)
> type NoteList = [(PitchClass, Octave)]
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]
> type Triad = [Int]
> type Scale = [Int]

///Tror att vi ska ha major i våran KEY så att den ser ut så här istället Key = (C, Major)/////


> cmaj = (C, Major)
> fmaj = (F, Major)
> gmaj = (G, Major)


> fd d n = n d v
> vol  n = n   v
> v      = [Volume 100]
> lmap f l = line (map f l)

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

progression :: Key -> [Integer]

///BASS///

> type BassStyle = [(Int, Dur)]

--> basic = cycle [(0,hn),(4,hn)]
--> calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
--> boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]

Only basic atm

autoBass bs key cp = autoBass cp cl
	

> autoBass :: BassStyle -> Scale -> [(PitchClass, Dur)] -> Music
> autoBass [] _ _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass _ _ [] = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass (b:bl) sc (c:cl) = foldr1 (:=:) (handleRest c b sc) :+: autoBass bl sc cl

> handleRest c1 b1 sc1
> 	|fst b1 == -1 = [Rest (snd b1)]
> 	|otherwise = [Note (fst note, pitch ) (snd b1) [Volume 65]]
> 	where
> 	note = (!!) notes  (mod (((!!) sc1 (fst b1)) + (fromJust $ lookupInt notes (fst c1))) 12)
> 	pitch = 3 + div (fromJust $ lookupInt notes (fst c1)) 12

Note (x, 4) (1%2) [Volume 60]
13/12 = 1   3+notesPosition/12
b1a = lmap (fd hn) [c  3, g 3, f  3, g 3]

///CHORDS///

> findTriad :: Key -> PitchClass -> Triad
> findTriad (key, mode) note
> 	| mode == Major = [0,4,7]
> 	| otherwise = [0,3,7]								
																					
If C, [0,4,7] -> [C,E,G]

> findPitch rootNote position = fromJust $ lookupNote notes (mod ( (fromJust $ lookupInt notes rootNote) + position) 12)

> noteList = take 16 $ drop 52 notes

> createChord :: PitchClass -> Triad -> NoteList															
> createChord _ [] = []											
> createChord n (p:ps) = (findPitch n p, div noteInt 12) :  createChord n ps
> 	where noteInt = fromJust $ lookupInt noteList $ findPitch n p



This maps some notes to a chord. 

> mapChord :: NoteList -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note x dur [Volume 60] | x <- chord ]

///AUTOMUSIC///



stairwayChords = [(A, Minor), hn]

AutoBass creates the bass line of the song.
autoBass :: BassStyle -> Key -> ChordProgression -> Music

AutoChord generates the chords of the song.

																					Needs to be updated

> autoChord :: Key -> ChordProgression -> [Music]
> autoChord _ [] = [] 
> autoChord rootKey ((note,dur):keys) = (mapChord (createChord note $ findTriad rootKey note) dur) : autoChord rootKey keys


autoComp creates a song with a baseline and chords.

> autoComp :: ChordProgression -> Key -> Music
> autoComp cp key = (Instr "piano" $ Tempo 2 $ (foldr1 (:+:) (autoChord key cp))) :=: (Tempo 2 $ autoBass calypso ionian cp)

MIGHT COME IN HANDY



> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

