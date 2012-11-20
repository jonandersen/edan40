\section{Haskore CSound Tutorial}
\label{csound-tut}

> type Scale = [Int]
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]


AutoBass creates the bass line of the song.
autoBass :: BassStyle -> Key -> ChordProgression -> Music


AutoChord generates the chords of the song.
autoChord :: Key -> ChordProgression -> Music

autoComp creates a song with a baseline and chords.
autoComp :: ChordProgression -> Key -> Music





twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords
