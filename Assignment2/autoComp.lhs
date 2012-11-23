\section{Haskore CSound Tutorial}
\label{csound-tut}

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 

///UTIL STUFF///

> type Key = (PitchClass, Mode)
> type NoteList = [(PitchClass, Octave)]
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]
> type Triad = [Int]
> twinkleChords = [(C, wn) ,(F , hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, wn), (F, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn)]

///Tror att vi ska ha major i våran KEY så att den ser ut så här istället Key = (C, Major)/////


> cmaj = (C, Major)
> fmaj = (F, Major)
> gmaj = (G, Major)


> fd d n = n d v
> vol  n = n   v
> v      = [Volume 100]
> lmap f l = line (map f l)

Dessa tva borde vi kunna gora battre, eller hitta en fardig funktion som gor detta?

> lookupts :: [(PitchClass, Int)] -> Int -> PitchClass
> lookupts ((t1,t2):ts) m
>		| m == t2 = t1
>		| otherwise = lookupts ts m


> lookuptf :: [(PitchClass, Int)] -> PitchClass -> Int
> lookuptf ((t1,t2):ts) m
>		| m == t1 = t2
>		| otherwise = lookuptf ts m


> notes = zip [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B][0,1..]

Our model for chords, can be expanded

progression :: Key -> [Integer]

///BASS///

> type BassStyle = [(Int, Dur)]
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]

Only basic atm

autoBass bs key cp = basicPattern cp cl
	

> basicPattern :: [(PitchClass, Dur)] -> BassStyle -> Scale -> Music
> basicPattern [] _ _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> basicPattern _ [] _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> basicPattern (c:cl) (b:bl) sc = foldr1 (:=:) (handleRest c b sc) :+: basicPattern cl bl sc

> handleRest c1 b1 sc1
> 	|fst b1 == -1 = [Rest (snd b1)]
> 	|otherwise = [Note (fst note, pitch ) (snd b1) [Volume 80]]
> 	where
> 	note = (!!) notes  (mod (((!!) sc1 (fst b1)) + (lookuptf notes (fst c1))) 12)
> 	pitch = 3 + div (lookuptf notes (fst c1)) 12



> twinkleWithBasicBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ basicPattern (splitToBasicChord twinkleChords) basic ionian) 
> twinkleWithBoogieBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ basicPattern (splitToBoogie twinkleChords) boogie ionian) 
> twinkleWithCalypsoBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ basicPattern (splitToCalypso twinkleChords) calypso ionian) 

> splitToBasicChord :: [Chord] -> [Chord] 
> splitToBasicChord [] = []
> splitToBasicChord (x:xs)
>		| snd x == wn = splitPair ++ splitToBasicChord xs
>		| otherwise  = x: splitToBasicChord xs
> 	where
> 	splitPair = [(fst x, hn), (fst x, hn)]

> splitToCalypso :: [Chord] -> [Chord]
> splitToCalypso [] = []
> splitToCalypso (x:xs)
> 	| snd x == wn = totalSplit ++ splitToCalypso xs
> 	| snd x == hn = partialSplit ++ splitToCalypso xs
> 	where
> 	totalSplit = [(fst x, qn), (fst x, en),(fst x, en),(fst x, qn), (fst x, en),(fst x, en)]
> 	partialSplit = [(fst x, qn),(fst x, en), (fst x, en)]

> splitToBoogie :: [Chord] -> [Chord]
> splitToBoogie [] = []
> splitToBoogie (x:xs)
> 	| snd x == wn = splitEight ++ splitToBoogie xs
> 	| snd x == hn = splitQuarter ++ splitToBoogie xs
> 	where
> 	splitEight = [(fst x, en), (fst x, en),(fst x, en), (fst x, en),(fst x, en), (fst x, en),(fst x, en), (fst x, en)]
> 	splitQuarter = [(fst x, en), (fst x, en),(fst x, en), (fst x, en)]

Note (x, 4) (1%2) [Volume 60]
13/12 = 1   3+notesPosition/12
b1a = lmap (fd hn) [c  3, g 3, f  3, g 3]

///CHORDS///

> findTriad :: Key -> PitchClass -> Triad
> findTriad (key, mode) note
> 	| mode == Major = [0,4,7]
> 	| otherwise = [0,3,7]								
																					
If C, [0,4,7] -> [C,E,G]

> createChord :: PitchClass -> Triad -> NoteList															
> createChord _ [] = []											
> createChord n (p:ps) = ((lookupts notes (mod ((lookuptf notes n) + p) 12 )), 4) : createChord n ps

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
> autoComp cp key = Instr "piano" $ Tempo 2 $ (foldr1 (:+:) (autoChord key cp))


> twinkleWithChords = twinkleMelody :=: autoComp twinkleChords cmaj 

twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords


> v1a = lmap (fd qn) [c 4,c 4, g 4,g 4 ,a 4,a 4]
> v1g = lmap vol [g 4 hn]
> v1b = lmap (fd qn) [f 4 , f 4, e 4,e 4, d 4,d 4]
> v1c = lmap vol [c 4 hn]
> v1 = v1a :+: v1g :+: v1b :+: v1c

> v2a = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
> v2d = lmap vol [d 4 hn]
> v2b = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
> v2 = v2a :+: v2d :+: v2a :+: v2d
 
> twinkleMelody =  Instr "piano"$ Tempo 2 $ v1 :+: v2 :+: v1


///TESTS///

> testGetChord = (createChord F [0,4,7])
> testGetChord2 = (createChord C [1])

> testSplitWholeChord1 = splitToBasicChord [(C, wn)]
> testSplitWholeChord2 = splitToBasicChord [(C, hn)]
> testSplitWholeChord3 = splitToBasicChord twinkleChords

> testBasicPattern = basicPattern (splitToBasicChord twinkleChords)  basic ionian
> testBoogiePattern = basicPattern (splitToBoogie twinkleChords)  boogie ionian
> testSplitToCalypso = splitToCalypso twinkleChords
> testSplitToBoogie = splitToBoogie twinkleChords

MIGHT COME IN HANDY

> type Scale = [Int]
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

Position		Major chord		Minor chord
1						Ionian		
2						Mixolydian		Dorian
3													Phrygian
4						Lydian		
5						Mixolydian		
6													Aeolian
7

