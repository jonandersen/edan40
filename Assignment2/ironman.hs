import Haskore
import AutoComp

rep x 0 = []
rep x n= x ++ rep x (n-1)

song = lmap vol [b 3 qn, d 4 qn, d 4 en, e 4 en, e 4 qn, g 4 sn, fs 4 sn,g 4 sn, fs 4 sn, g 4 sn, fs 4 sn, d 4 en, d 4 en, e 4 en, e 4 qn]
rest = enr
voice = times 3 (song :+: rest)

ironChords =  rep [(B, hn) ,(E , hn), (G, hn), (E, hn),(B, hn) ,(E , hn), (G, hn), (E, hn),(B, hn) ,(E , hn), (G, hn), (E, hn)] 3
ironman = Instr "Church Organ" $ Tempo 1 $ voice


ironBasic   = ironman :=: autoComp basic  ironChords (C, Major)
ironCalypso = ironman :=: autoComp calypso  ironChords (C, Major)
ironBoogie  = ironman :=: (Instr "Church Organ" $ autoComp boogie ironChords (C, Major))