-- EndCh10.hs
--
-- End of chapter exercises (ch 10)

module EndCh10 where


-- Vowels and Consonants
--
-- Given the following sets of consonants and vowels:
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- a)
-- Write a function that produces 3-tuples for
-- all possible stop-vowel-stop combinations
svs :: [(Char,Char,Char)]
svs = [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

-- b)
-- Modify svs so that it only returns
-- combinations that begin with 'p'
psvs :: [(Char,Char,Char)]
psvs = [(s,v,s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

-- c)
-- Set up lists of nouns and verbs, and write a
-- function that produces 3-tuples representing
-- possible noun-verb-noun sentences
nouns = ["program", "beet", "keycap", "headphones", "coffee", "record"]
verbs = ["listen", "obtain", "consider", "lean", "loafe"]

nvn :: [(String,String,String)]
nvn = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]


-- Less concrete variation of svs and nvn functions
xyx :: [a] -> [b] -> [(a,b,a)]
xyx xs ys = [(x,y,x') | x <- xs, y <- ys, x' <- xs]
