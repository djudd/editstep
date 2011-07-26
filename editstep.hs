--import Data.Set (Set)
--import qualified Data.Set as Set
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
        reverse $ fst $ foldl addEditSteps ([], root) words

addEditSteps (graph, dictionary) word = 
        let nextDictionary = {-# SCC "nextDictionary" #-} insert dictionary word
            nextGraph = {-# SCC "nextGraph" #-} (generateEditSteps dictionary word) ++ graph 
        in (nextGraph, nextDictionary)

generateEditSteps dictionary word =
        zip (repeat word) $ members dictionary $ permutations word

members dictionary words = {-# SCC "members" #-} filter (contains dictionary) words

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

-- trie types & constants
data Trie = Node Char [Trie] | Leaf deriving (Show)

eow = '\0'
root = Node eow []

-- trie utility functions
getLetter (Node letter _) = letter
getLetter Leaf = eow

getChildren (Node _ children) = children
getChildren Leaf = []

isLeaf Leaf = True
isLeaf (Node _ _) = False

hasLetter letter Leaf = False
hasLetter letter (Node value _) = value == letter

findChild (Node _ children) letter =
        List.find (hasLetter letter) children

hasLeaf (Node _ children) = 
        List.any isLeaf children

-- trie insertion
insert :: Trie -> String -> Trie
insert node [] =
        if hasLeaf node 
        then node
        else Node (getLetter node) $ Leaf:(getChildren node)
insert node (letter:remaining) = 
        case findChild node letter of
                Just child -> modifyChild node child remaining
                Nothing -> addChildren node letter remaining

modifyChild :: Trie -> Trie -> String -> Trie
modifyChild parent child remaining =
        let isOther = \candidate -> (getLetter candidate) /= (getLetter child)
            otherChildren = filter isOther (getChildren parent)
            modifiedChild = insert child remaining
        in setChildren parent (modifiedChild:otherChildren)

addChildren :: Trie -> Char -> String -> Trie
addChildren node letter remaining = 
        let children = getChildren node
        in case remaining of
                [] -> setChildren node $ (Node letter [Leaf]):children
                (x:xs) -> setChildren node $ (addChildren (Node letter []) x xs):children

setChildren (Node letter _) children =
        Node letter children

-- trie lookup
contains :: Trie -> String -> Bool
contains node [] = hasLeaf node
contains node (letter:remaining) =
        case findChild node letter of
                Just child -> contains child remaining
                Nothing -> False

-- performance test code
fiveLetterWords = [a:b:c:d:e:[] | a <- alphabet, b <- alphabet, c <- alphabet, d <- alphabet, e <- alphabet]
println str = putStr $ (show str) ++ "\n"

-- for comparison: egrep -c '^[[:alpha:]]{5}$' dev-challenges/001-edit-step-ladder/input/big
lookupAllFiveLetterWordsInDictionary = do
        contents <- getContents
        println $ length fiveLetterWords
        println $ length $ filter (contains (foldl insert root (lines contents))) fiveLetterWords
