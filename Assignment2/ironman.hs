import Haskore
import AutoComp

rep x 0 = []
rep x n= x ++ rep x (n-1)

song = lmap vol [b 2 qn, d 3 qn, d 3 en, e 3 en, e 3 qn, g 3 sn, fs 3 sn,g 3 sn, fs 3 sn, g 3 sn, fs 3 sn, d 3 en, d 3 en, e 3 en, e 3 qn]
rest = enr
voice = times 3 (song :+: rest)

ironChords =  rep [(B, hn) ,(E , hn), (G, hn), (E, hn),(B, hn) ,(E , hn), (G, hn), (E, hn),(B, hn) ,(E , hn), (G, hn), (E, hn)] 3
ironman = Instr "Church Organ" $ Tempo 1 $ voice


ironBasic   = ironman :=: autoComp basic  ironChords (C, Major)
ironCalypso = ironman :=: autoComp calypso  ironChords (C, Major)
ironBoogie  = ironman :=: (Instr "Church Organ" $ autoComp boogie ironChords (C, Major))