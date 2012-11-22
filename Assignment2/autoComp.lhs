\section{Haskore CSound Tutorial}
\label{csound-tut}

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 

///UTIL STUFF///

> type Key = (PitchClass, Mode)
> type NoteList = [PitchClass]
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


> notes = [(C, 0),(Cs, 1), (D, 2),(Ds, 3),(E, 4), (F, 5),(Fs, 6),(G, 7),(Gs, 8),(A, 9), (As, 10), (B, 11) ]

Our model for chords, can be expanded

progression :: Key -> [Integer]

///BASS///

> type BassStyle = [(Int, Dur)]
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]

Only basic atm

autoBass bs key cp = basicPattern cp cl
	

> basicPattern :: [(PitchClass, Dur)] -> BassStyle -> Music
> basicPattern [] _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> basicPattern _ [] = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> basicPattern (c:cl) (b:bl) = foldr1 (:=:) [Note (note, pitch ) (snd b) [Volume 80]] :+: basicPattern cl bl
> 	where
> 	note = getSingleChord (fst c) (fst b)
> 	pitch = 3 + div (lookuptf notes (fst c)) 12

> twinkleWithBasicBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ basicPattern (splitWholeChord twinkleChords) basic) 


> splitWholeChord :: [Chord] -> [Chord] 
> splitWholeChord [] = []
> splitWholeChord (x:xs)
>		| snd x == wn = splitPair ++ splitWholeChord xs
>		| otherwise  = x: splitWholeChord xs
> 	where
> 	splitPair = [(fst x, hn), (fst x, hn)]


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
> createChord n (p:ps) = (lookupts notes (mod ((lookuptf notes n) + p) 12 )) : createChord n ps

> getSingleChord :: PitchClass -> Int -> PitchClass
> getSingleChord n p = (lookupts notes (mod ((lookuptf notes n) + p) 12 ))

This maps some notes to a chord. 

> mapChord :: NoteList -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note (x, 4) dur [Volume 60] | x <- chord ]

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

> testSplitWholeChord1 = splitWholeChord [(C, wn)]
> testSplitWholeChord2 = splitWholeChord [(C, hn)]
> testSplitWholeChord3 = splitWholeChord twinkleChords

> testBasicPattern = basicPattern (splitWholeChord twinkleChords)  basic


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

