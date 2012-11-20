\section{Haskore CSound Tutorial}
\label{csound-tut}

> import Haskore
> import Ratio 

type Key = [Note]

> type Scale = [Int]
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

Position		Major chord		Minor chord
1		Ionian		
2		Mixolydian		Dorian
3									Phrygian
4		Lydian		
5		Mixolydian		
6									Aeolian
7

> prog = [0,2,4]

> getChord :: Int -> Int -> [PitchClass]
> getChord 1 _ = [C,E,G]
> getChord 4 _ = [F,A,C]

> mapChord :: [PitchClass] -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note (x, 4) dur [Volume 100] | x <- chord ]
> song = foldr1 (:+:) [(mapChord (getChord 1 0) (wn)), (mapChord (getChord 4 0) (hn)), (mapChord (getChord 1 0) (hn))]
> twinkle = Instr "piano" song



(C, wn), (F, hn)

getChord :: [Note] -> Key -> [Note]
C 1, F 1/2 C 1/2, G 1/2. C 1/2, G 1/2 C 1/2
C 1/2 G 1/2, C 1/2 G 1/2, C 1/2 G 1/2, C 1/2 G 1/2
C 1, F 1/2 C 1/2, G 1/2 C 1/2, G 1/2 C 1/2

ChordProgression 


> type BassStyle = [(Int, Ratio Int)]
> basic = [(0,hn),(4,hn)]
> calypso = [(-1,1%4),(0, en),(2, en), (-1, 1%4),(0,en),(2,en)]
> boogie = [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]

AutoBass creates the bass line of the song.
autoBass :: BassStyle -> Key -> ChordProgression -> Music


AutoChord generates the chords of the song.
autoChord :: Key -> ChordProgression -> Music

autoComp creates a song with a baseline and chords.
autoComp :: ChordProgression -> Key -> Music







twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords
