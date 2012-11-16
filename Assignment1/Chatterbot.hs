module Chatterbot where
import Utilities
import Pattern
import Random
import Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine

      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)

      if (not.endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
   r <- randomIO :: IO Float
   return (rulesApply (takeOne r brain))

-- Picks one random answer for each question
takeOne :: (RealFrac r) => r -> BotBrain -> [PhrasePair]
takeOne _ [] = []            
takeOne r (x:xs) = (fst x, pick r (snd x)): (takeOne r xs)


rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply [] _ = []
rulesApply xs p = try (transformationsApply "*" (reflect) xs) p

reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs) = (try (flip lookup reflections) x ): reflect xs

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile [] = []
rulesCompile (x:xs) = ((words . map toLower) $ fst x, listCompile $ snd x) : rulesCompile xs

listCompile :: [String] -> [Phrase]
listCompile [] = []
listCompile (y:ys) = (words y) : listCompile ys 
--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply xs p = fix (try (transformationsApply "*" id xs)) p
-- Fix ensure that all occurences are replaced.

