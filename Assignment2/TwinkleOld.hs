module Twinkle where
import Haskore
import Ratio 

 -- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)
 
-- repeat something n times
times  1    m = m
times (n+1) m = m :+: (times n m)
 
-- Baseline:
b1a = lmap (fd hn) [c  3, g 3, f  3, g 3]
b1b = lmap (fd hn) [g  3, c 4, g 3, c 4]
b1 = b1a :+: b1b
b2 = lmap (fd hn) [c 3, d 3, c  3, d 3]

bassLine = b1 :+: times 2 b2 :+: b1

cbc = times 2 (qnr :+: lmap (fd en) [c 3, e 3])
cbfc = qnr :+: lmap (fd en) [f 3, a 4] :+: qnr :+: lmap (fd en) [c 3, e 3]
cbgc = qnr :+: lmap (fd en) [g 3, b 4] :+: qnr :+: lmap (fd en) [c 3, e 3]
cbcg = qnr :+: lmap (fd en) [c 3, e 3] :+: qnr :+: lmap (fd en) [g 3, b 4]
cbFirstLine = cbc :+: cbfc :+: times 2 cbgc

calypsoBassLine = cbFirstLine :+: times 4 cbcg :+: cbFirstLine

bb1a = lmap (fd en) [c 3 , g 3 , a 4 , g 3 , c 3 , g 3 , a 4 , g 3]
bb1b = lmap (fd en) [f 3 , c 4 , d 4 , c 4 , c 3 , g 3 , a 4 , g 3]
bb1c = lmap (fd en) [g 3 , d 4 , e 4 , d 4 , c 3 , g 3 , a 4 , g 3]
bb1 = bb1a :+: bb1b :+: times 2 bb1c :+: bb1a :+: bb1b

bb2 = lmap (fd en) [c 3 , g 3 , a 4 , g 3, g 3 , d 4 , e 4 , d 4 ]
--bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
boogieBassLine = bb1 :+: bb2 :+: bb1
-- Main Voice:
v1a = lmap (fd qn) [c 4,c 4, g 4,g 4 ,a 4,a 4]
v1g = lmap vol [g 4 hn]
v1b = lmap (fd qn) [f 4 , f 4, e 4,e 4, d 4,d 4]
v1c = lmap vol [c 4 hn]
v1 = v1a :+: v1g :+: v1b :+: v1c

v2a = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
v2d = lmap vol [d 4 hn]
v2b = lmap (fd qn) [g 4,g 4,f 4,f 4,e 4,e 4]
v2 = v2a :+: v2d :+: v2a :+: v2d
 
mainVoice =  v1 :+: v2 :+: v1
 
--Chords
cMajor = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[C, E, G] ]
fMajor = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[F, A, C] ]
gMajor = foldr1 (:=:) [ Note (x, 4) (1%2) [Volume 60] | x<-[G, B, D] ]
cMajorLong = foldr1 (:=:) [ Note (x, 4) (1%1) [Volume 60] | x<-[C, E, G] ]

first = cMajorLong :+: fMajor :+: cMajor :+: gMajor :+: cMajor :+: gMajor :+: cMajor
second = times 4 (cMajor :+: gMajor)

chords = first :+: second :+: first
-- Putting it all together:
twinkle = Instr "piano" (Tempo 2 (Phrase [Dyn SF] chords :=: mainVoice))
twinkleBass = Instr "piano" (Tempo 2 (Phrase [Dyn SF] chords :=:bassLine :=:mainVoice))
twinkleCalypsoBass = Instr "piano" (Tempo 2 (Phrase [Dyn SF] chords :=:calypsoBassLine :=:mainVoice))
twinkleBoogieBass = Instr "piano" (Tempo 2 (Phrase [Dyn SF] chords :=: boogieBassLine :=:mainVoice))