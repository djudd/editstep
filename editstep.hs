import EditStepGraph
import qualified Trie as Trie

main = printLongestLadder

printLongestLadder = do
        contents <- getContents
        println $ longestEditStepLadder $ lines contents

printEditStepGraph = do
        contents <- getContents
        println $ editStepGraph $ lines contents

-- to get expected result: egrep -c '^[[:alpha:]]{5}$' wordlists/big
printNumberOfFiveLetterWords = do
        contents <- getContents
        println $ length fiveLetterWords
        println $ length $ allFiveLetterWordsInDictionary $ lines contents

alphabet = "abcdefghijklmnopqrstuvwxyz"
fiveLetterWords = [a:b:c:d:e:[] | a <- alphabet, b <- alphabet, c <- alphabet, d <- alphabet, e <- alphabet]
inDictionary dictionary word = Trie.contains word $ foldl Trie.insert Trie.empty dictionary
allFiveLetterWordsInDictionary dictionary = filter (inDictionary dictionary) fiveLetterWords

println x = putStr $ (show x) ++ "\n"


