EDAN40 - Functional Music
Jon Andersen ada08jan@student.lu.se 
Marcus Carlberg ada08mca@student.lu.se

1.Introduction
AutomComp is a program that given a list of Chords and a Key of a song can generate 3 different types of bass lines and chords to a song. To implement
this program we use a musical library called Haskore. 

2.Library
The name of this module is AutoComp, this name has to be imported to the
song file otherwise the autoComp function would not be compiled and made
executable. Haskore stands for most of the musical theory in this project 
and is used to describe songs in a program. Hiding key felt necessary because we wanted to have our own definition of it mentioned further down in this test. Ratio is needed to describe ratios and since all note's duration is described in parts of a bar in Haskore this module was needed. Maybe is used to handle lookups.

> module AutoComp where
> import Haskore hiding (Key)
> import Ratio 
> import Maybe
> import Ix

3.Types
To make everything work definitions had to be made for the different musical terms.
The first thing we need to define is the Pitchclass. This attribute decides which tone 
a Note belongs to and there are 21 different PitchClasses defined in Haskore. 
All tones circulates in the frequency domain. When you double the frequency
you get the same tone again but in a brighter sound. This phenomena is called that we have 
raised the note one Octave. 

There is a huge overlap in Notes and to cover all possible notes we only need to define 12 notes in 
a list called a NoteList. A common way to find notes that sound good when used together is to use
predefined scales. A Scale in this program is a list of Int's where every entry in the list correspond 
to a good choice of Notes from our NoteList.

Example: We have our note list consisting of [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] and we want to create a song 
in ionian scale defined as follow [0, 2, 4, 5, 7, 9, 11]. Following this scale we would get the notes 
[C,D,E,F,G,A,B] to use in our song.

A Triad is 3 entries from our scales that are used to create a Chord. In this program we concentrate on Chords consisting 
of 3 Notes only therefor the name Triad suffices. In a more general application there is Chords consisting 
of more and less notes than 3.   

The key of a song is defined as a PitchClass and a Mode. The Pitchclass tells us which note we want as our starting point is our 
scale. If we have PitchClass = E then when we pick index 0 from our NoteList we will get an E. The mode of the song is either in 
Major or Minor scale. If a song is in Major the ionian, lydian and mixolydian scales are used and if the song is in Minor scale 
the aeolian, dorian and phrygian scales are used. 

When you write a song you are interested in which Chord you are going to play and for how long you are going to play that Chord. 
Therefor the Chord type are defined analogous and a list of Chords is defined as a ChordProgression. 


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
 

4.Utility
Here follows some helper functions that are used in different ways through the 
project. Mostly to map different notes to each other and thus creating chords and to look up notes in lists in different ways.

-- note updaters for mappings

> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
 
-- repeat something n times

> times  1    m = m
> times (n+1) m = m :+: (times n m)

Given an Indexed list of Notes sometimes you would like to retrieve which PitchClass a certain
index have and given a certain PitchClass get the position in the list where you could find the PitchClass. These methods are used to accomplish this 
We make the assumption that if it's Nothing the program is not valid and hence
fromJust is used throughout the program.  

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


5.Bass
The first task was to create a BassLine given the Kay of the song, the Chords of the song and
which BassStyle that we want to use. First of all a BassStyle is defined as a list of Int's and Dur's.
The Int's represent relative to the current Chord that we are using, how many steps we should move forward in our NoteList 
when selecting which bass tone we want to play. The Dur's represent the duration of this tone. 
All the BassStyles are looped so they will be played in the same manner through the whole song with different notes.


> type BassStyle = [(Int, Dur)]

The three different BassStyles that we use is basic, calypso and boogie. 

Basic consists of 2 half notes where the first is just the same as the current Chord and the second is the fourth note in the scale that we use. 

Calypso starts with a quarter note rest and then two eight notes. The rest is represented here as a -1 so that the 
program can identify that it should not create a new Note but instead a Rest. The first of the two eight notes are 
based on the current Chord and the second is based on the second note in the scale that we use.

Boogie bass is just a repetition of eight notes in the pattern 0, 4, 5, 4 and these are used in analogous way as mentioned above.

> basic, calypso, boogie :: BassStyle
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1, qn),(0, en),(2, en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en)]


Our main goal in this section is to implement the autoBass function and to do that we need some helpers functions.
Since our BassStyles are strictly defined the songs that goes into the program most often doesn't match and therefore
the need of a function that can divid our ChordProgression into a ChordProgression that fits the bass pattern arises. 
The splitChord function solves that problem provided the BassStyle to be used and the songs ChordProgression it generates 
the formatted ChordProgression. This function is used in autoComp before the calling of autoBass.


> splitChord :: BassStyle -> ChordProgression -> ChordProgression 
> splitChord [] _ = []
> splitChord _ [] = []
> splitChord (b:bl) (c:cl)
> 	| snd c > snd b = splitChord (b:bl) ([(fst c, (snd c)/2), (fst c, (snd c)/2)] ++ cl)
> 	| otherwise =   c: splitChord bl cl

In a song you would want to use different scales depending on if the song are in Major or Minor chords.
To get the song to sound better we can choose which scale to use according to the relative position that
we are in the scale. For the Major scales the three different BassStyles are independent on ionian, mixolydian and lydian scale
and therefor we don't need to consider them in this example. In the Minor scales the Dorian scale separate from the others
and must therefor be handled. The proper way to do this is to check the songs PitchClass and compare it to the second position
in one of the Minor scale, if these match we use Dorian scale and if it doesn't match we can use aeolian or phrygian independently.

Position		Major chord		Minor chord
1						Ionian		
2						Mixolydian		Dorian
3													Phrygian
4						Lydian		
5						Mixolydian		
6													Aeolian
7

> getScale :: Key -> Chord ->Scale
> getScale k c
> 	| snd k == Major = ionian
>  	| otherwise = isDorian k c
> 	where
> 	isDorian :: Key -> Chord -> Scale
> 	isDorian k c
> 		| (fromJust $ lookupInt notes (fst c)) == ((fromJust $ lookupInt notes (fst k)) + 3) = dorian
> 		| otherwise = aeolian


The calypso bass has a rest in it and that need to be handled to. That is what the handleRest function solves.

> handleRest :: Chord -> (Int, Dur) -> Scale -> [Music]
> handleRest c1 b1 sc1
> 	|fst b1 == -1 = [Rest (snd b1)]
> 	|otherwise = [Note (fst note, pitch ) (snd b1) [Volume 65]]
> 	where
> 	note = (!!) notes  (mod (((!!) sc1 (fst b1)) + (fromJust $ lookupInt notes (fst c1))) 12)
> 	pitch = 3 + div (fromJust $ lookupInt notes (fst c1)) 12

AutoBass uses these helper functions recursively to generate a Music object of the whole bass line given the BassStyle, the Kay and ChordProgression. 

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass [] _ _ = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass _ _ [] = foldr1 (:=:) [Note (C,4) 0 [Volume 0]]
> autoBass bl k cl = generateBass bl k (splitChord bl cl)
>	where 
>	generateBass (b:bl) k (c:cl) = foldr1 (:=:) (handleRest c b (getScale k c)) :+: autoBass bl k cl

6. Chords
We define a chord as a triad and a duration. We work with chords as ints and only when creating
the chord with the duration we map it back to the PitchClass and a Octave. As of this
we will only pass triads to the functions. 

We have some basic rules of thumbs that will make the end result sound somewhat better.

1. All chord notes should be picked from a limited interval. In our case (E:4 -> G:5).
	 We make this constraint by limiting what notes can be chosen from our notes to the 
	 following:

> noteList = take 13 $ drop 52 notes
> intList = take 13 $ drop 52 [0,1..]

2. We want to minimize the distance of the note in the new chord from the note in the previous played chord. If
   the previous played chord was C [0,4,7] and the new chord to be played is a D
   we want to play [2,6,9] and not [14,18,21].

3. The final rule of thumb is to make the chords tighter. This means that we want to keep the chord notes as close 
   to each other as possible. For example we want to play [0,4,7] as opposed to [0,4,19].

As we only use three notes for our chord, we can see that Major and Minor chords only have one outcome each.

> findTriad :: Key -> PitchClass -> Triad
> findTriad (key, mode) note
> 	| mode == Major = [0,4,7]
> 	| otherwise = [0,3,7]								

There is a need to lookup notes and map them to in, we use the following helper functions.
As mentioned before our program is not valid if the note is not in the list. The program will throw an
exception if this happens. 

> findPitchInt :: NoteList -> PitchClass -> Int
> findPitchInt n pitchClass = (fromJust $ lookupInt n pitchClass)

> findPitch :: NoteList -> Int -> PitchClass
> findPitch n int = (fromJust $ lookupNote n int)

There is a need to take the current Note(From our melody) and the triad for the key of the song. 
If we have D -> [0,4,7] we will receive [2,6,9]. The first rule of thumb is applied as we ensure that 
the outcome is within this interval. All triad will be as low as possible this means that we will use 
[0,4,7] if possible instead of [12,16,19]. createChord accomplishes this.

> createChord :: PitchClass -> Triad -> Triad		
> createChord n triad = map (findPitchInt noteList) $ map (findPitch notes) $ map ((findPitchInt notes n) + ) triad

We want the chords to change as little as possible from the previous one. This is done on a one note basis. To accomplish this we generate
all possible permutations for the chord, we then ensure that we still follow rule 1. Finally we select the chords which are the closest to 
our previous chord. 

> permutations = [[0,0,0],
> 								[12,0,0],
> 								[0,12,0],
> 	 							[0,0,12],
> 								[12,12,0],
> 								[12,0,12],
> 								[0,12,12],
> 	 							[12,12,12]]

> applyPermutations :: Triad -> [Triad]
> applyPermutations triad = filterInRange $ map (zipWith (+) triad) permutations

> filterInRange :: [Triad] -> [Triad]
> filterInRange = filter (all (inRange (52,64)))

> getMinimum triads f = minimum $ map f triads

> pickClosest :: Triad -> [Triad] -> [Triad]
> pickClosest triad triads = filter(\x -> closeSum triad x <= (getMinimum triads (closeSum triad))) triads

> closeSum :: Triad -> Triad -> Int
> closeSum x y = abs $ sum $ zipWith (-) x y 

As it happens several chords can be obtain that are close to the previous one. We want to select the chords that have the smallest
inner difference. That is we want to select the chord as rule 3 describe. There can still be several chords that are tight, and if this
happens we just selects the first.

> pickTightest :: [Triad] -> Triad
> pickTightest triads = head $ filter (\x -> tightSum x <= (getMinimum triads tightSum)) triads

> tightSum :: Triad -> Int
> tightSum triad = (maximum triad) - (minimum triad)

Since all chords has been represented as triads we need to map them back to chords so Haskore can play them properly.
																					
> convertToNote :: Triad -> NoteList
> convertToNote [] = []
> convertToNote (x:xs) = (fromJust $ lookupNote noteList x, div x 12):convertToNote xs

> mapChord :: NoteList -> Dur -> Music
> mapChord chord dur = foldr1 (:=:) [ Note x dur [Volume 60] | x <- chord ]

CreateChords applies all the above functions to create a list of music. It makes an recursive call for each of 
the notes in the chord progression. For each call it passes the current chord to the next to ensure that
rule of thumb 2 can be made. 

> createChords :: Key -> ChordProgression -> Triad -> [Music] 
> createChords _ [] _ = []																
> createChords rootKey ((note,dur):keys) previous = (mapChord (convertToNote current) dur) : createChords rootKey keys current
>    where current = pickTightest $ pickClosest previous$  applyPermutations $ createChord note $ findTriad rootKey note															
																					
AutoChord creates the first chord of the song and then calls the createChords function to create the rest. 

> autoChord :: Key -> ChordProgression -> Music
> autoChord rootKey cp = Instr "piano" $ foldr1 (:+:) $ createChords rootKey cp headChord
>    where headChord = pickTightest $ applyPermutations $ createChord (fst $head cp) $ findTriad rootKey (fst $ head cp)

7. Automusic

To generate an accompaniment there is a need to combine the bass, the cords and the song of the song in a nice manner. It is important that the "correct" chords are used for the song to sound good and it is preferable that the bass is handled in a "correct" manner also. The "" in "correct" is used because there are many different ways to chose chords in songs and we are only doing the theoretically correct way. When we have all of these attributes we can combine them to a finished song by the use of autoComp.

> autoComp :: BassStyle -> ChordProgression -> Key -> Music
> autoComp bs cp key =  (autoChord key cp) :=: (autoBass bs key cp)


