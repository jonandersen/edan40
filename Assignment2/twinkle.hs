module Twinkle where
import Haskore
import AutoComp

basic = cycle [(0,hn),(4,hn)]
calypso = cycle [(-1, qn),(0, en),(2, en), (-1, qn),(0,en),(2,en)]
boogie = cycle [(0,en),(4,en),(5,en),(4,en),(0,en),(4,en),(5,en),(4,en)]
ionian = [0, 2, 4, 5, 7, 9, 11]
lydian = [0, 2, 4, 6, 7, 9, 11	]
mixolydian = [0, 2, 4, 5, 7, 9, 10]
aeolian = [0, 2, 3, 5, 7, 8, 10]
dorian = [0, 2, 3, 5, 7, 9, 10]
phrygian = [0, 1, 3, 5, 7, 8, 10]

twinkleChords = [(C, wn) ,(F , hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, wn), (F, hn), (C, hn), (G, hn), (C, hn), (G, hn), (C, hn)]

twinkleWithBasicBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ autoBass  basic ionian (splitToBasicChord twinkleChords)) 
twinkleWithBoogieBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ autoBass  boogie ionian (splitToBoogie twinkleChords)) 
twinkleWithCalypsoBass = twinkleMelody :=: (Instr "piano" $ Tempo 2 $ autoBass  calypso ionian (splitToCalypso twinkleChords)) :=: autoComp twinkleChords cmaj


twinkleWithChords = twinkleMelody :=: autoComp twinkleChords cmaj 
--twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
--twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
--twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords


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



testGetChord = (createChord F [0,4,7])
testGetChord2 = (createChord C [1])

testSplitWholeChord1 = splitToBasicChord [(C, wn)]
testSplitWholeChord2 = splitToBasicChord [(C, hn)]
testSplitWholeChord3 = splitToBasicChord twinkleChords

testBasicPattern = autoBass basic ionian (splitToBasicChord twinkleChords)
testBoogiePattern = autoBass boogie ionian (splitToBoogie twinkleChords)
testSplitToCalypso = splitToCalypso twinkleChords
testSplitToBoogie = splitToBoogie twinkleChords