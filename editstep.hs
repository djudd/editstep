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
fiveLetterWords = [a:b:c:d:e:[] | a <- alphabet, b <- alphabet, c <- alphabet, d <- alphabet, e <- alphabet]
allFiveLetterWordsInDictionary dictionary = filter (Trie.contains (foldl Trie.insert Trie.empty dictionary)) fiveLetterWords
printNumberOfFiveLetterWords = do
        contents <- getContents
        println $ length fiveLetterWords
        println $ length $ allFiveLetterWordsInDictionary $ lines contents

println x = putStr $ (show x) ++ "\n"


