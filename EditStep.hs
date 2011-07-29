module EditStep 
( longestEditStepLadder
) where

import Trie

longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let (longestLadder, _) = foldl insertTrackingLongestPath (0, Trie.empty) words
        in longestLadder

insertTrackingLongestPath :: (Int, Trie) -> String -> (Int, Trie)
insertTrackingLongestPath (longestPath, trie) word =
        let longestPathToWord = longestPathTo word trie
            trie' = insert longestPathToWord word trie
            longestPath' = max longestPath longestPathToWord
        in (longestPath', trie')            

longestPathTo word trie =
        (+1) $ maximum $ map longestPathToPermutationAt $ zip (suffixes word) (suffixSubtries trie word)

suffixes word = [drop n word | n <- [0..(length word)]]

suffixSubtries node word = scanl subtrie node word

subtrie node letter = case findChild node letter of
        Just child -> child
        Nothing -> Node letter [] 0

longestPathToPermutationAt (suffix, node) = 
        maximum $ (longestPathToSubstitutionAt suffix node):(longestPathToAdditionBefore suffix node):(longestPathToDeletionAt suffix node):[]

longestPathToSubstitutionAt [] node = 0
longestPathToSubstitutionAt (letter:suffix) node =
        longestPathContaining suffix $ otherChildren node letter

longestPathToAdditionBefore suffix node =
        longestPathContaining suffix $ getChildren node

longestPathToDeletionAt [] node = 0
longestPathToDeletionAt (_:suffix) node =
        Trie.lookup suffix node

longestPathContaining suffix nodes =
        if null nodes
        then 0
        else maximum $ map (Trie.lookup suffix) nodes
