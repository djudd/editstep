import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

main = printLongestLadders

printEditStepGraph = do
        contents <- getContents
        putStr $ show $ editStepGraph $ List.sort $ lines contents

printLongestLadder = do
        contents <- getContents
        putStr $ (show $ longestEditStepLadder $ lines contents) ++ "\n"

longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let edges = editStepGraph $ List.sort words
        in maximum $ map length $ Map.elems $ foldl calcLongestPath Map.empty edges

calcLongestPath :: Map String [String] -> (String, String) -> Map String [String]
calcLongestPath paths (stepEnd, stepStart) = 
        let startPath = maybe [stepStart] id (Map.lookup stepStart paths)
            endPath = maybe [stepEnd] id (Map.lookup stepEnd paths)
        in (if (length startPath) < (length endPath)
            then paths 
            else Map.insert stepEnd (stepEnd : startPath) paths)

editStepGraph :: [String] -> [(String, String)]
editStepGraph words =
        fst $ foldl addEditSteps ([], Set.empty) words

addEditSteps :: ([(String, String)], Set String) -> String -> ([(String, String)], Set String)
addEditSteps (graph, dictionary) word = 
        let nextDictionary = Set.insert word dictionary
            nextGraph = graph ++ (editSteps dictionary word)
        in (nextGraph, nextDictionary)

editSteps :: Set String -> String -> [(String, String)]
editSteps dictionary word =
        zip (repeat word) (filter ((flip Set.member) dictionary) (Set.toList $ permutations word))

-- TODO: replace zipWith & repeat with fold & currying, for all below
permutations :: String -> Set String
permutations word =
        Set.union 
        (Set.fromList $ concat $ zipWith substitutionsAndDeletions [0..(length word)-1] (repeat word))
        (Set.fromList $ concat $ zipWith additions [0..(length word)] (repeat word))

alphabet = "abcdefghijklmnopqrstuvwxyz"

additions :: Int -> String -> [String]
additions pos word = 
        zipWith3 add alphabet (repeat pos) (repeat word)

substitutionsAndDeletions :: Int -> String -> [String] 
substitutionsAndDeletions pos word = 
        let others = Nothing : (map Just $ filter (/= (word !! pos)) alphabet)
        in zipWith3 substitute others (repeat pos) (repeat word)

-- TODO: optimize below
add :: Char -> Int -> String -> String
add letter pos word =
        (take pos word) ++ [letter] ++ (drop pos word)

substitute :: Maybe Char -> Int -> String -> String
substitute letter pos word = 
        let insert = case letter of Nothing -> []
                                    Just letter -> [letter]
        in (take pos word) ++ insert ++ (drop (pos+1) word)



