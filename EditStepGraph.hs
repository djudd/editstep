module EditStepGraph 
( editStepGraph,
  longestEditStepLadder
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
                (generateEditSteps nextDictionary word) ++ graph 
        in (nextGraph, nextDictionary)

generateEditSteps dictionary word =
        zip (repeat word) $ (Trie.permutations word dictionary)

