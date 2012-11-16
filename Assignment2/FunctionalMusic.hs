module FunctionalMusic where
import Haskore
import Ratio 

-- type Pitch = (PitchClass, Octave)
-- data PitchClass = Cf | C | Cs | Df | D | Ds | Ef |
--                    E | Es | Ff | F | Fs | Gf | G |
--                   Gs | Af | A | As | Bf | B | Bs
--     deriving (Eq,Ord,Ix,Show)
-- type Octave = Int

-- "piano"
-- "vibes"
-- "bass"
-- "flute"
-- "sax"
-- "guitarr"
-- "violin"
-- "violins"
-- "drums"

-- data Music = Note Pitch Dur [NoteAttribute] -- a note
--            | Rest Dur           -- a rest (pause)
--            | Music :+: Music    -- sequential composition
--            | Music :=: Music    -- parallel composition
--            | Tempo (Ratio Int) Music  -- scale the tempo
--            | Trans Int Music    -- transpose (raise 
--                                 -- or lower) all notes
--            | Instr IName Music  -- select instrument
--      deriving (Show, Eq)

-- autoBass :: BassStyle -> Key -> ChordProgression -> Music
-- 
-- autoChord :: Key -> ChordProgression -> Music

c4 = Note (C, 4) (1%4) [Volume 80]
Instr "flute" (c4 :+: c4 :+: c4)
cMajor = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[C, E, G] ]


-- twinkleBasic = twinkleMelody :=: autoComp basic (C, Major) twinkleChords