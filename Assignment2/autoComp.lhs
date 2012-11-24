\section{Haskore CSound Tutorial}
\label{csound-tut}

EDAN40 - Functional Music
Jon Andersen ada08jan@student.lu.se 
Marcus Carlberg ada08mca@student.lu.se

Introduction
AutomComp is a program that given a list of Chords and a Key of a song can generate 
a 3 different types of bass lines and chords to a song. 

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 
> import Maybe

///UTIL STUFF///

To make everything work defenitions had to be made for the different musical terms.
The first thing we need to define is the Pitchclass. This attribute decides whitch tone 
a Note belongs to and there are 21 different PitchClasses defined in Haskore. 
All tones circulates in the frequency domain. When you double the frequency
you get the same tone again but in a brighter sound. This fenomenom is called that we have 
raised the note one Octave. 

There is a huge overlap in Notes and to cover all possible notes we only need to define 12 notes in 
a list called a NoteList. A common way to find notes that sound good when used together is to use
predefined scales. A Scale in this program is a list of Int's where every entry in the list correspond 
to a good choise of Notes from our NoteList.

Example: We have our notelist consisting of [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] and we want to create a song 
in ionian scale defined as follow [0, 2, 4, 5, 7, 9, 11]. Following this scale we would get the notes 
[C,D,E,F,G,A,B] to use in our song.

A Triad is 3 entries from our scales that are used to create a Chord. In this program we consentrate on Chords consisting 
of 3 Notes only therefor the name Triad suffices. In a more general application there is Chords consisting 
of more and less notes than 3.   

The key of a song is definied as a PitchClass and a Mode. The Pitchclass tells us which note we want as our starting point is our 
scale. If we have PitchClass = E then when we pick index 0 from our NoteList we will get an E. The mode of the song is either in 
Major or Minor scale. If a song is in Major the ionian, lydian and mixolydian scales are used and if the song is in Minor scale 
the aeolian, dorian and phrygian scales are used. 

When you write a song you are interested in which Chord you are going to play and for how long you are going to play that Chord. 
Therefor are the Chord type defiened analogous and a list of Chords is defined as a ChordProgression. 

> type Note = (PitchClass, Octave)
> type NoteList = [Note]
> type Scale = [Int]
> type Triad = [Int]
> type Key = (PitchClass, Mode)
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]

Major Scales

> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11	]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]

Minor Scales

> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

NoteList

> notes = zip (cycle [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]) [0,1..]
 

-- note updaters for mappings

> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
 
-- repeat something n times

> times  1    m = m
> times (n+1) m = m :+: (times n m)

Given an Indexed list of Notes sometimes you would like to retrive which PitchClass a certain
index have and Given a certain PitchClass get the possition in the list where you could find the PitchClass

> lookupNote :: NoteList -> Int -> Maybe PitchClass
> lookupNote [] _ = Nothing
> lookupNote ((t1,t2):ts) m
>		| m == t2 = Just t1
>		| otherwise = lookupNote ts m


> lookupInt :: NoteList -> PitchClass -> Maybe Int
> lookupInt [] _ = Nothing
> lookupInt ((t1,t2):ts) m
>		| m == t1 = Just t2
>		| otherwise = lookupInt ts m


///BASS///

The first task was to create a BassLine given the Kay of the song, the Chords of the song and
which BassStyle that we want to use. First of all a BassStyle is defined as a list of Int's and Dur's.
The Int's represent relative to the current Chord that we are using, how many steps we should move forward in our NoteList 
when selecting which basstone we want to play currently. The Dur's represent how long time we want to play this tone. 
All the BassStyles are looped so they will be played in the same manner through the whole song whit different notes.

> type BassStyle = [(Int, Dur)]

The three different BassStles that we use is basic, calyps and boogie. Basic konsists of 2 half notes where the first is just 
the same as the current Chord and the second is the fourth note in the scale that we use. 

Calypso starts with a quater note rest and then two eight notes. The rest is represented here as a -1 so that the 
program can identify that it should not create a new Note but instead a Rest. The first of the two eight notes are 
baed on the current Chord and the second is based on the second note in the scale that we use.

Boogie bass is just a repetiorion of eight notes in the pattern 0, 4, 5, 4 and these are used in analogous way as mentioned above.

> basic, calypso, boogie :: BassStyle
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en)]


Our main goal in this section is to implement the autoBass function and to do that we need some helpers functions.

> splitChord :: BassStyle -> ChordProgression -> ChordProgression 
> splitChord [] _ = []
> splitChord _ [] = []
> splitChord (b:bl) (c:cl)
> 	| snd c > snd b = splitChord (b:bl) ([(fst c, (snd c)/2), (fst c, (snd c)/2)] ++ cl)
> 	| otherwise =   c: splitChord bl cl

> getScale :: Key -> Chord ->Scale
> getScale k c
> 	| snd k == Major = ionian
>  	| otherwise = isDorian k c
> 	where
> 	isDorian :: Key -> Chord -> Scale
> 	isDorian k c
> 		| (fromJust $ lookupInt notes (fst c)) == ((fromJust $ lookupInt notes (fst k)) + 3) = dorian
> 		| otherwise = aeolian


> handleRest :: Chord -> (Int, Dur) -> Scale -> [Music]
> handleRest c1 b1 sc1
> 	|fst b1 == -1 = [Rest (snd b1)]
> 	|otherwise = [Note (fst note, pitch ) (snd b1) [Volume 65]]
> 	where
> 	note = (!!) notes  (mod (((!!) sc1 (fst b1)) + (fromJust $ lookupInt notes (fst c1))) 12)
> 	pitch = 2 + div (fromJust $ lookupInt notes (fst c1)) 12


> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass [] _ _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass _ _ [] = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass (b:bl) k (c:cl) = foldr1 (:=:) (handleRest c b (getScale k c)) :+: autoBass bl k cl








///CHORDS///

> findTriad :: Key -> PitchClass -> Triad
> findTriad (key, mode) note
> 	| mode == Major = [0,4,7]
> 	| otherwise = [0,3,7]								
																					
> convertToNote :: Triad -> NoteList
> convertToNote [] = []
> convertToNote (x:xs) = (fromJust $ lookupNote noteList x, div x 12):convertToNote xs

> findPitchInt :: NoteList -> PitchClass -> Int
> findPitchInt n pitchClass = (fromJust $ lookupInt n pitchClass)

> findPitch :: NoteList -> Int -> PitchClass
> findPitch n int = (fromJust $ lookupNote n int)

> noteList = take 13 $ drop 52 notes

> createChord :: PitchClass -> Triad -> Triad		
> createChord n triad = map (findPitchInt noteList) $ map (findPitch notes) $ map ((findPitchInt notes n) + ) triad

Assumption made is (E:4 -> G:5)

> makeCloser :: Triad -> Triad -> Triad 
> makeCloser [] [] = []
> makeCloser (prev:prevs) (curr:currs) 
> 	| elem new (take 13 $ drop 52 [0,1..]) && abs (new - prev) < abs (curr - prev) = new : makeCloser prevs currs
> 	| otherwise = curr : makeCloser prevs currs
> 	where new = curr + 12

> makeTigther :: Triad -> Triad
> makeTigther triad = triad

This maps some notes to a chord. 

> mapChord :: NoteList -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note x dur [Volume 60] | x <- chord ]

///AUTOMUSIC///

AutoChord generates the chords of the song.

																					Needs to be updated

> createChords :: Key -> ChordProgression -> Triad -> [Music] 
> createChords _ [] _ = []																
> createChords rootKey ((note,dur):keys) previous = (mapChord (convertToNote current) dur) : createChords rootKey keys current
>    where current = makeTigther $ makeCloser previous $ createChord note $ findTriad rootKey note															
																					

> autoChord :: Key -> ChordProgression -> Music
> autoChord rootKey cp = Instr "piano" $ Tempo 2 $ foldr1 (:+:) $ createChords rootKey cp headChord
>    where headChord = (makeTigther $ createChord (fst $head cp) $ findTriad rootKey (fst $ head cp))


autoComp creates a song with a baseline and chords.


> autoComp :: BassStyle -> ChordProgression -> Key -> Music
> autoComp bs cp key =  (autoChord key cp) :=: (Tempo 2 $ autoBass bs key $ splitChord bs cp)



