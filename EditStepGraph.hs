module EditStepGraph 
( editStepGraph,
  longestEditStepLadder,
  alphabet
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Trie as Trie

-- traverse edit steph graph
longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let edges = {-# SCC "buildGraph" #-} 
                editStepGraph $ List.sort words
            longestPathsToNodes = {-# SCC "longestPathsToNodes" #-} 
                Map.elems $ foldl longestPathToEdgeEnd Map.empty edges
        in maximum $ map length longestPathsToNodes

longestPathToEdgeEnd paths (stepEnd, stepStart) = 
        let startPath = pathToNode paths stepStart
            endPath = pathToNode paths stepEnd
        in (if (length startPath) < (length endPath)
            then paths 
            else Map.insert stepEnd (stepEnd : startPath) paths)

pathToNode paths node =
        maybe [node] id $ Map.lookup node paths

-- build edit step graph
editStepGraph :: [String] -> [(String, String)]
editStepGraph words =
        reverse $ fst $ foldl addEditSteps ([], Trie.empty) $ List.sort words

addEditSteps (graph, dictionary) word = 
        let nextDictionary = {-# SCC "nextDictionary" #-} 
                Trie.insert dictionary word
            nextGraph = {-# SCC "nextGraph" #-} 
                (generateEditSteps dictionary word) ++ graph 
        in (nextGraph, nextDictionary)

generateEditSteps dictionary word =
        zip (repeat word) $ members dictionary $ permutations word

members dictionary words = {-# SCC "members" #-} 
        filter (Trie.contains dictionary) words

-- permutation generation (candidate edit steps, not necessarily real words)
permutations word = {-# SCC "generatePermutations" #-} 
        prependAllDeletions word $ prependAllAdditions word $ prependAllSubstitutions word []

alphabet = "abcdefghijklmnopqrstuvwxyz"

prependAllAdditions word permutations = 
        foldl (prependLetterAdditions word) permutations alphabet

prependLetterAdditions word permutations letter = 
        foldl (prependAddition word letter) permutations [0..(length word)]

prependAddition word letter permutations pos = 
        trimLeadingDuplicate $ (add word letter pos):permutations

add word letter pos = {-# SCC "add" #-} 
        (take pos word) ++ [letter] ++ (drop pos word)

prependAllSubstitutions word permutations = 
        foldl (prependLetterSubstitutions word) permutations alphabet

prependLetterSubstitutions word permutations letter = 
        foldl (prependSubstitution word letter) permutations [0..(length word)-1]

prependSubstitution word letter permutations pos
        | letter == (word !! pos) = permutations
        | otherwise               = (substitute word letter pos):permutations

substitute word letter pos = {-# SCC "sub" #-} 
        (take pos word) ++ [letter] ++ (drop (pos+1) word)

prependAllDeletions word permutations =
        foldl (prependDeletion word) permutations [0..(length word)-1]

prependDeletion word permutations pos = 
        trimLeadingDuplicate $ (delete word pos):permutations

delete word pos = {-# SCC "del" #-} 
        (take pos word) ++ (drop (pos+1) word)

trimLeadingDuplicate [] = []
trimLeadingDuplicate (x:[]) = [x]
trimLeadingDuplicate (x:y:xs) 
        | x == y    = y:xs
        | otherwise = x:y:xs
