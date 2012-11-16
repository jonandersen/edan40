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
c4long = Note (C, 4) (1%2) [Volume 80]
g4 = Note (G, 4) (1%4) [Volume 80]
a4 = Note (A, 4) (1%4) [Volume 80]
g4long = Note (G, 4) (1%2) [Volume 80]
f4 = Note (F, 4) (1%4) [Volume 80]
e4 = Note (E, 4) (1%4) [Volume 80]
d4 = Note (D, 4) (1%4) [Volume 80]
--g4long = Note (G, 4) (1%2) [Volume 80]
fl = Instr "flute" (c4 :+: c4 :+: c4)
cMajor = foldr1 (:=:) [ Note (x, 4) (1%4) [Volume 60] | x<-[C, E, G] ]
eMajor = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[E, Gs, B] ]
dMajor = foldr1 (:=:) [ Note (x, 4) (1%4) [Volume 60] | x<-[D, A, E] ]
cMajorLong = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[C, E, G] ]
 
song = [cMajor, dMajor, eMajor,cMajor, dMajor, eMajor, dMajor,dMajor,dMajor,dMajor, cMajorLong, cMajorLong]
song2 = [c4,c4, g4,g4 ,a4,a4, g4long, f4, f4, e4,e4, d4,d4, c4long]
musik = foldr1 (:+:) song
musik2 = foldr1 (:+:) song2
musik3 = Tempo 4 musik2
spanien = Instr "guitarr" musik
blinka = Instr "guitarr" musik3


-- twinkleBasic = twinkleMelody :=: autoComp basic (C, Major) twinkleChords