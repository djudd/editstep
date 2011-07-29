import EditStep

main = printLongestLadder

printLongestLadder = do
        contents <- getContents
        print $ longestEditStepLadder $ lines contents

