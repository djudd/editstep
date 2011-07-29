module Trie 
( longestEditStepLadder
) where

import qualified Data.List as List
import Data.Ord
import Data.Maybe

data Trie = Node Char [Trie] | Leaf Int deriving (Show)

-- utility functions operating on single nodes
eow = '\0'
empty = Node eow []

getLetter (Node letter _) = letter
getLetter (Leaf _) = eow

getChildren (Node _ children) = children
getChildren (Leaf _) = []

isLeaf (Leaf _) = True
isLeaf (Node _ _) = False

hasLetter letter (Leaf _) = False
hasLetter letter (Node value _) = value == letter

findChild (Leaf _) letter = Nothing
findChild (Node _ children) letter = {-#SCC "findChild" #-}
        List.find (hasLetter letter) children

hasLeaf (Leaf _) = False
hasLeaf (Node _ children) = 
        List.any isLeaf children

setChildren (Node letter _) children =
        Node letter children

-- basic trie operations
insert :: Int -> String -> Trie -> Trie
insert val [] (Node letter children) = {-# SCC "insert" #-}
        let fertileChildren = filter (not . isLeaf) children
        in Node letter (Leaf val:fertileChildren)
insert val (letter:remaining) node = {-# SCC "insert" #-}
        case findChild node letter of
                Just child -> modifyChild node child remaining val
                Nothing -> addSubTrie node letter remaining val

modifyChild parent child remaining val =
        let isOther = \candidate -> (getLetter candidate) /= (getLetter child)
            otherChildren = filter isOther (getChildren parent)
            modifiedChild = insert val remaining child
        in setChildren parent (modifiedChild:otherChildren)

addSubTrie node letter remaining val = 
        let children = getChildren node
            addChild = \child -> setChildren node $ child:children
        in case remaining of
                [] -> addChild $ Node letter [Leaf val]
                (x:xs) -> addChild $ addSubTrie (Node letter []) x xs val

lookup :: String -> Trie -> Int
lookup [] (Leaf _) = {-# SCC "lookup" #-} 0
lookup [] (Node _ children) = {-# SCC "lookup" #-}
        case List.find isLeaf children of
                Just (Leaf val) -> val
                Nothing -> 0
lookup (letter:remaining) node = {-# SCC "lookup" #-}
        case findChild node letter of
                Just child -> Trie.lookup remaining child
                Nothing -> 0

-- edit step specific
longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let (longestLadder, _) = foldl insertTrackingLongestPath (0, Trie.empty) words
        in longestLadder

insertTrackingLongestPath :: (Int, Trie) -> String -> (Int, Trie)
insertTrackingLongestPath (longestPath, trie) word =
        let longestPathToWord = {-# SCC "longestPathToWord" #-} longestPathTo word trie
            trie' = {-# SCC "nextTrie" #-} insert longestPathToWord word trie
            longestPath' = max longestPath longestPathToWord
        in (longestPath', trie')            

longestPathTo word trie =
        (+1) $ maximum $ map longestPathToPermutationAt $ zip (suffixes word) (suffixSubtries trie word)

suffixes word = [drop n word | n <- [0..(length word)]]

suffixSubtries node word = scanl subtrie node word

subtrie node letter = case findChild node letter of
        Just child -> child
        Nothing -> Node letter []        

longestPathToPermutationAt (suffix, node) = 
        maximum $ (longestPathToSubstitutionAt suffix node):(longestPathToAdditionBefore suffix node):(longestPathToDeletionAt suffix node):[]

longestPathToSubstitutionAt [] node = 0
longestPathToSubstitutionAt (letter:suffix) node =
        let otherChildren = filter (not . (hasLetter letter)) $ getChildren node
        in longestPathContaining suffix otherChildren

longestPathToAdditionBefore suffix node =
        longestPathContaining suffix $ getChildren node

longestPathToDeletionAt [] node = 0
longestPathToDeletionAt (_:suffix) node =
        Trie.lookup suffix node

longestPathContaining suffix nodes =
        if null nodes
        then 0
        else maximum $ map (Trie.lookup suffix) nodes
