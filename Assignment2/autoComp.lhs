\section{Haskore CSound Tutorial}
\label{csound-tut}


AutoBass creates the bass line of the song.
\begin{verbatim}
> autoBass :: BassStyle -> Key -> ChordProgression -> Music
\end{verbatim}

AutoChord generates the chords of the song.
\begin{verbatim}
> autoChord :: Key -> ChordProgression -> Music
\end{verbatim}

autoComp creates a song with a baseline and chords.
\begin{verbatim}
> autoComp :: ChordProgression -> Key -> Music
\end{verbatim}




\begin{verbatim}
> twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
> twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
> twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords
\end{verbatim}
