-- Programmer exercise (ch 11)

data OperatingSystem = Linux | OpenBds | Mac | Windows
    deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [Linux, OpenBds, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
    [ Programmer { os = x, lang = y}
    | x <- allOperatingSystems
    , y <- allLanguages
    ]
