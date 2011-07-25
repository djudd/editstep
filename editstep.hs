import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

main = printLongestLadder
--main = printEditStepGraph

printEditStepGraph = do
        contents <- getContents
        putStr $ show $ editStepGraph $ List.sort $ lines contents

printLongestLadder = do
        contents <- getContents
        putStr $ (show $ longestEditStepLadder $ lines contents) ++ "\n"

longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let edges = {-# SCC "buildGraph" #-} editStepGraph $ List.sort words
        in {-# SCC "calLongestPath" #-} maximum $ map length $ Map.elems $ foldl calcLongestPath Map.empty edges

calcLongestPath :: Map String [String] -> (String, String) -> Map String [String]
calcLongestPath paths (stepEnd, stepStart) = 
        let startPath = maybe [stepStart] id (Map.lookup stepStart paths)
            endPath = maybe [stepEnd] id (Map.lookup stepEnd paths)
        in (if (length startPath) < (length endPath)
            then paths 
            else Map.insert stepEnd (stepEnd : startPath) paths)

editStepGraph :: [String] -> [(String, String)]
editStepGraph words =
        reverse $ fst $ foldl addEditSteps ([], Set.empty) words

addEditSteps (graph, dictionary) word = 
        let nextDictionary = {-# SCC "nextDictionary" #-} Set.insert word dictionary
            nextGraph = {-# SCC "nextGraph" #-} (generateEditSteps dictionary word) ++ graph 
        in (nextGraph, nextDictionary)

generateEditSteps dictionary word =
        zip (repeat word) $ members dictionary $ permutations word

members dictionary words = {-# SCC "members" #-} filter (member' dictionary) words
member' = flip Set.member

permutations :: String -> [String]
permutations word =
        {-# SCC "generatePermutations" #-} prependAllDeletions word $ prependAllAdditions word $ prependAllSubstitutions word []

alphabet = "abcdefghijklmnopqrstuvwxyz"

prependAllAdditions word permutations = 
        foldl (prependLetterAdditions word) permutations alphabet

prependLetterAdditions word permutations letter = 
        foldl (prependAddition word letter) permutations [0..(length word)]

prependAddition word letter permutations pos = 
        trimLeadingDuplicate $ (add word letter pos):permutations

add :: String -> Char -> Int -> String
add word letter pos =
        {-# SCC "add" #-} (take pos word) ++ [letter] ++ (drop pos word)

prependAllSubstitutions word permutations = 
        foldl (prependLetterSubstitutions word) permutations alphabet

prependLetterSubstitutions word permutations letter = 
        foldl (prependSubstitution word letter) permutations [0..(length word)-1]

prependSubstitution word letter permutations pos
        | letter == (word !! pos) = permutations
        | otherwise               = (substitute word letter pos):permutations

substitute :: String -> Char -> Int -> String
substitute word letter pos =
        {-# SCC "sub" #-} (take pos word) ++ [letter] ++ (drop (pos+1) word)

prependAllDeletions word permutations =
        foldl (prependDeletion word) permutations [0..(length word)-1]

prependDeletion word permutations pos = 
        trimLeadingDuplicate $ (delete word pos):permutations

delete :: String -> Int -> String
delete word pos = 
        {-# SCC "del" #-} (take pos word) ++ (drop (pos+1) word)

--trimLeadingDuplicate :: [String] -> [String]
trimLeadingDuplicate [] = []
trimLeadingDuplicate (x:[]) = [x]
trimLeadingDuplicate (x:y:xs) 
        | x == y    = y:xs
        | otherwise = x:y:xs
