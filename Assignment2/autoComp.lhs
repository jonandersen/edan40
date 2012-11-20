\section{Haskore CSound Tutorial}
\label{csound-tut}


\begin{verbatim}
> type Bass = [int]
> basic = [1,1,1,1,5,5,5,5]
> calypso = [0,0,1,3,0,0,1,3]
> boogie = [1,5,6,5,1,5,6,5]

AutoBass creates the bass line of the song.
> autoBass :: BassStyle -> Key -> ChordProgression -> Music

AutoChord generates the chords of the song.
> autoChord :: Key -> ChordProgression -> Music

autoComp creates a song with a baseline and chords.
> autoComp :: ChordProgression -> Key -> Music




> twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
> twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
> twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords
\end{verbatim}
