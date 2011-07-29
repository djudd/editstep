import qualified Trie as Trie

main = printLongestLadder

printLongestLadder = do
        contents <- getContents
        println $ Trie.longestEditStepLadder $ lines contents

-- to get expected result: egrep -c '^[[:alpha:]]{5}$' wordlists/big
printNumberOfFiveLetterWords = do
        contents <- getContents
        println $ length fiveLetterWords
        println $ length $ allFiveLetterWordsInDictionary $ lines contents

alphabet = "abcdefghijklmnopqrstuvwxyz"
fiveLetterWords = [a:b:c:d:e:[] | a <- alphabet, b <- alphabet, c <- alphabet, d <- alphabet, e <- alphabet]
inDictionary dictionary word = Trie.contains word $ foldl (flip (Trie.insert 1)) Trie.empty dictionary
allFiveLetterWordsInDictionary dictionary = filter (inDictionary dictionary) fiveLetterWords

println x = putStr $ (show x) ++ "\n"


