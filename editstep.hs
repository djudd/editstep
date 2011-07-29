import Trie

main = printLongestLadder

printLongestLadder = do
        contents <- getContents
        print $ longestEditStepLadder $ lines contents

