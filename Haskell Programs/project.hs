import Control.Monad
import Control.Exception
import System.IO
import System.Directory
import System.Random
import Data.Char

-- RULE SETS ---------------------------------------------------

r_Sentence = ["S+V","PS+PV","ES+V","PES+PV"]
r_Object = ["S","PS"]
r_Verb = ["TV","IV",{-"LV",-}"hv+TV","hv+IV"]
r_PlVerb = ["PTV", "PIV", {-"PLV",-} "phv+PTV", "phv+PIV"]
r_Subject = [""]
r_PlSubject = ["pd+ADV+ADJ+ADJ+PN", "ADV+ADJ+ADJ+PN"]
r_ExSubject = [""]
r_PlExSubject = [""]	
r_Noun = ["n","n+n"]
r_PlNoun = ["pn","n+pn"]
r_TVerb = [""]
r_PlTVerb = [""]
r_IVerb = [""]
r_PlIVerb = [""]
r_LVerb = ["lv+ADV+adj", "ADV+lv+adj", "lv+ADV+P", "ADV+lv+P"]
r_PlLVerb = ["plv+ADV+adj", "ADV+plv+adj", "plv+ADV+P", "ADV+plv+P"]
r_Adverb = ["adv", ""]
r_Adjective = ["adj", ""]
r_Prep = ["prep", ""]

-- MAIN --------------------------------------------------------

main = do 
    -- BUILD WORD MASTER LIST
    words <- buildWordSet
    -- GENERATE SENTENCE
    sentence <- generate r_Sentence words genSentence
    -- PUT TO SCREEN
    putStrLn sentence

    return ()

-- FILE RETRIEVAL / INIT FUNCTIONS -----------------------------
----------------------------------------------------------------	
	
readIn :: String -> IO [String]
readIn fileName = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do 
            contents <- readFile fileName
            let words = lines contents
            return words
        else do
            putStrLn $ "ERROR: " ++ fileName ++ " does NOT exist."
            return []

buildWordSet :: IO [[String]]
buildWordSet = do
    --                                          -- Index in Master List
    adj <- readIn "adjectives.txt"              -- 0 - Adjectives
    adv <- readIn "adverbs.txt"                 -- 1 - Adverbs
    det <- readIn "determiners.txt"             -- 2 - Determiners
    hV <- readIn "helpingVerbs.txt"             -- 3 - Helping Verbs
    iV <- readIn "intransitiveVerbs.txt"        -- 4 - Intrans Verbs
    lV <- readIn "linkingVerbs.txt"             -- 5 - Linking Verbs
    n <- readIn "nouns.txt"                     -- 6 - Nouns
    pD <- readIn "pluralDeterminers.txt"        -- 7 - Plural Determiners
    pHV <- readIn "pluralHelpingVerbs.txt"      -- 8 - Plural Helping Verbs
    pIV <- readIn "pluralIntransitiveVerbs.txt" -- 9 - Plural Intrans Verbs
    pLV <- readIn "pluralLinkingVerbs.txt"      -- 10 - Plural Linking Verbs
    pN <- readIn "pluralNouns.txt"              -- 11 - Plural Nouns
    pTV <- readIn "pluralTransitiveVerbs.txt"   -- 12 - Plural Trans Verbs
    p <- readIn "prepositions.txt"              -- 13 - Prepositions
    tV <- readIn "transitiveVerbs.txt"          -- 14 - Trans Verbs
     
    let words = adj : adv : det : hV : iV : lV : n : pD : pHV : pIV : pLV : pN : pTV : p : tV : []
    return words

--       words       result
pick :: [String] -> IO String
pick ws = do
    gen <- newStdGen
    let	(index, newgen) = randomR (0, (length ws)-1) gen
        word = (ws!!index)
    return word

--           rules        words              generation function             result
generate :: [String] -> [[String]] -> (String -> [[String]] -> IO String) -> IO String
generate rules ws f = do
    rule <- pick rules
    word <- f rule ws
    return word
	
generateB :: [String] -> [[String]] -> (String -> [[String]] -> IO (String, Bool)) -> IO (String, Bool)
generateB rules ws f = do
    rule <- pick rules
    result <- f rule ws
    return result	

-- GENERATION FUNCTIONS ----------------------------------------
-- w/ simple string output
-- t: String -> [[String]] -> IO String
--     rule       words        result
----------------------------------------------------------------

genSentence :: String -> [[String]] -> IO String
genSentence rule words = do
    case rule of
        "S+V"       -> do
                        subject         <- generate r_Subject words genSubject
                        (verb, needObj) <- generateB r_Verb words genVerb
                        object          <- generate r_Object words genObject
                        if needObj
                        then return $ subject ++ verb ++ object
                        else return $ subject ++ verb
        "PS+PV"     -> do
                        psubject         <- generate r_PlSubject words genPlSubject
                        (pverb, needObj) <- generateB r_PlVerb words genPlVerb
                        object           <- generate r_Object words genObject
                        if needObj
                        then return $ psubject ++ pverb ++ object
                        else return $ psubject ++ pverb
        "ES+V"      -> do
                        esubject        <- generate r_ExSubject words genExSubject
                        (verb, needObj) <- generateB r_Verb words genVerb
                        object          <- generate r_Object words genObject
                        if needObj
                        then return $ esubject ++ verb ++ object
                        else return $ esubject ++ verb
        "PES+PV"    -> do
                        pesubject        <- generate r_PlExSubject words genPlExSubject
                        (pverb, needObj) <- generateB r_PlVerb words genPlVerb
                        object           <- generate r_Object words genObject
                        if needObj
                        then return $ pesubject ++ pverb ++ object
                        else return $ pesubject ++ pverb
        _           -> return "ERROR GENERATING SENTENCE"

genObject :: String -> [[String]] -> IO String
genObject rule words =
    case rule of
        "S"  -> generate r_Subject words genSubject
        "PS" -> generate r_PlSubject words genPlSubject
		
genSubject :: String -> [[String]] -> IO String
genSubject rule words = do
    let dets = (words!!2)
        adjs = (words!!0)
    det     <- pick dets
    adv     <- generate r_Adverb words genAdverb
    adj     <- pick adjs
    adj1    <- generate r_Adjective words genAdjective
    adj2    <- generate r_Adjective words genAdjective
    n       <- generate r_Noun words genNoun
    case adv of
        ""  -> return $ det ++ " " ++ adj1 ++ adj2 ++ n
        _   -> return $ det ++ " " ++ adv ++ adj ++ " " ++ adj2 ++ n
    

genPlSubject :: String -> [[String]] -> IO String
genPlSubject rule words = do
    let pdets = (words!!7)
        adjs  = (words!!0)
    pdet    <- pick pdets
    adv     <- generate r_Adverb words genAdverb
    adj     <- pick adjs
    adj1    <- generate r_Adjective words genAdjective
    adj2    <- generate r_Adjective words genAdjective
    pn      <- generate r_PlNoun words genPlNoun
    case rule of
        "pd+ADV+ADJ+ADJ+PN" ->  if   adv == ""
                                then return $ pdet ++ " " ++ adj1 ++ adj2 ++ pn
                                else return $ pdet ++ " " ++ adv ++ adj ++ " " ++ adj2 ++ pn
        "ADV+ADJ+ADJ+PN"    ->  if   adv == ""
                                then return $ adj1 ++ adj2 ++ pn
                                else return $ adv ++ adj ++ " " ++ adj2 ++ pn

genExSubject :: String -> [[String]] -> IO String
genExSubject rule words = do
    sub     <- generate r_Subject words genSubject
    hort    <- pick ["here","there"]
    return $ hort ++ " is " ++ sub ++ "that "

genPlExSubject :: String -> [[String]] -> IO String
genPlExSubject rule words = do
    psub    <- generate r_PlSubject words genPlSubject
    hort    <- pick ["here","there"]
    return $ hort ++ " are " ++ psub ++ "that "
	
genNoun :: String -> [[String]] -> IO String
genNoun rule words = do
    let nouns = (words!!6)
    n1 <- pick nouns
    n2 <- pick nouns
    case rule of
	   "n"     -> return $ n1 ++ " "
	   "n+n"   -> return $ n1 ++ " " ++ n2 ++ " "
		
genPlNoun :: String -> [[String]] -> IO String
genPlNoun rule words = do
    let nouns = (words!!6)
        pnouns = (words!!11)
    pn <- pick pnouns
    n  <- pick nouns
    case rule of
        "pn"    -> return $ pn ++ " "
        "n+pn"  -> return $ n ++ " " ++ pn ++ " "

genTVerb :: String -> [[String]] -> IO String
genTVerb rule words = do
    let tverbs = (words!!14)
    adv     <- generate r_Adverb words genAdverb
    tverb   <- pick tverbs
    prep    <- generate r_Prep words genPrep
    return $ adv ++ tverb ++ " " ++ prep


genPlTVerb :: String -> [[String]] -> IO String
genPlTVerb rule words = do
    let ptverbs = (words!!12)
    adv     <- generate r_Adverb words genAdverb
    ptverb  <- pick ptverbs
    prep    <- generate r_Prep words genPrep
    return $ adv ++ ptverb ++ " " ++ prep

genAdverb :: String -> [[String]] -> IO String
genAdverb rule words = do
    let adverbs = (words!!1)
    adverb <- pick adverbs
    case rule of
        "adv"   -> return $ adverb ++ " "
        ""      -> return ""

genAdjective :: String -> [[String]] -> IO String
genAdjective rule words = do
    let adjectives = (words!!0)
    adjective <- pick adjectives
    case rule of
        "adj"   -> return $ adjective ++ " "
        ""      -> return ""

genPrep :: String -> [[String]] -> IO String
genPrep rule words = do
    let preps = (words!!13)
    prep <- pick preps
    case rule of
        "prep"  -> return $ prep ++ " "
        ""      -> return ""

-- GENERATION FUNCTIONS ----------------------------------------
-- w/ (String, Bool) pair output
-- Bool represents necessity of Object in the sentence
-- t: String -> [[String]] -> IO (String, Bool)
--     rule       words             result
----------------------------------------------------------------

genVerb :: String -> [[String]] -> IO (String, Bool)
genVerb rule words = do
    let hverbs = (words!!3)
    hverb <- pick hverbs 
    case rule of
        "TV"    -> do str <- generate r_TVerb words genTVerb; return (str,True)
        "IV"    -> generateB r_IVerb words genIVerb
        --"LV"    -> generateB r_LVerb words genLVerb
        "hv+TV" -> do str <- generate r_PlTVerb words genPlTVerb 
                      return ((hverb ++ " " ++ str), True)
        "hv+IV" -> do (str,b) <- generateB r_PlIVerb words genPlIVerb;
                      return ((hverb ++ " " ++ str), b)
        _       -> return ("VERB_ERROR", False)

genPlVerb :: String -> [[String]] -> IO (String, Bool)
genPlVerb rule words = do
    let phverbs = (words!!8)
    phverb <- pick phverbs
    case rule of
        "PTV"       -> do str <- generate r_PlTVerb words genPlTVerb; return (str,True)
        "PIV"       -> generateB r_PlIVerb words genPlIVerb
        --"PLV"     ->
        "phv+PTV"   -> do str <- generate r_PlTVerb words genPlTVerb 
                          return ((phverb ++ " " ++ str), True)
        "phv+PIV"   -> do (str,b) <- generateB r_PlIVerb words genPlIVerb;
                          return ((phverb ++ " " ++ str), b)
        _           -> return ("VERB_ERROR", False)

genIVerb :: String -> [[String]] -> IO (String, Bool)
genIVerb rule words = do
    let iverbs = (words!!4)
    adv     <- generate r_Adverb words genAdverb
    prep    <- generate r_Prep words genPrep
    iverb   <- pick iverbs
    let flag = (prep /= "")
    return ((adv ++ iverb ++ " " ++ prep), flag)

genPlIVerb :: String -> [[String]] -> IO (String, Bool)
genPlIVerb rule words = do
    let piverbs = (words!!9)
    adv     <- generate r_Adverb words genAdverb
    prep    <- generate r_Prep words genPrep
    piverb  <- pick piverbs
    let flag = (prep /= "")
    return ((adv ++ piverb ++ " " ++ prep), flag)

genLVerb :: String -> [[String]] -> IO (String, Bool)
genLVerb rule words = do
    let lverbs = (words!!5)
    lverb   <- pick lverbs
    adv     <- generate r_Adverb words genAdverb
    prep    <- generate r_Prep words genPrep
    adj     <- generate r_Adjective words genAdjective
    case rule of
        "lv+ADV+adj"    -> return ((lverb ++ " " ++ adv ++ adj), False)
        "ADV+lv+adj"    -> return ((adv ++ lverb ++ " " ++ adj), False)
        "lv+ADV+P"      -> return ((lverb ++ " " ++ adv ++ prep), True)
        "ADV+lv+P"      -> return ((adv ++ lverb ++ " " ++ prep), True)

--genPlLVerb :: String -> [[String]] -> (IO String, Bool)