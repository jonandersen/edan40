import Haskore
import AutoComp

twinkleChords = [(C, wn) ,(F , hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, wn), (F, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn)]
twinkleMusic = twinkleMelody :=: autoComp boogie twinkleChords (C, Major)
teinkleChordTest = autoChord (C,Major) twinkleChords
-- twinkleWithChords = twinkleMelody :=: autoComp twinkleChords cmaj 
twinkleBasic   = twinkleMelody :=: autoComp basic  twinkleChords (C, Major)
twinkleCalypso = twinkleMelody :=: autoComp calypso  twinkleChords (C, Major)
twinkleBoogie  = twinkleMelody :=: autoComp boogie twinkleChords (C, Major) 


v1a = lmap (fd qn) [c 4,c 4, g 4,g 4 ,a 4,a 4]
v1g = lmap vol [g 4 hn]
v1b = lmap (fd qn) [f 4 , f 4, e 4,e 4, d 4,d 4]
v1c = lmap vol [c 4 hn]
v1 = v1a :+: v1g :+: v1b :+: v1c

v2a = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
v2d = lmap vol [d 4 hn]
v2b = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
v2 = v2a :+: v2d :+: v2a :+: v2d
 
twinkleMelody =  Instr "piano"$ Tempo 2 $ v1 :+: v2 :+: v1