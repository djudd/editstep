module Trie 
( longestEditStepLadder
) where

import qualified Data.List as List
import Data.Ord
import Data.Maybe

data Trie = Node Char [Trie] Int deriving (Show)

-- utility functions operating on single nodes
eow = '\0'
empty = Node eow [] 0

getLetter (Node letter _ _) = letter

getChildren (Node _ children _) = children

hasLetter l (Node letter _ _) = l == letter

findChild (Node _ children _) letter =
        List.find (hasLetter letter) children

otherChildren (Node _ children _) letter =
        filter (not . (hasLetter letter)) children

setChildren (Node letter _ val) children =
        Node letter children val

-- basic trie operations
insert :: Int -> String -> Trie -> Trie
insert val [] (Node letter children _) =
        Node letter children val
insert val (letter:remaining) node = 
        case findChild node letter of
                Just child -> modifyChild node child remaining val
                Nothing -> addSubtrie node letter remaining val

modifyChild node child remaining val =
        let modifiedChild = insert val remaining child
            others = otherChildren node $ getLetter child
        in setChildren node $ modifiedChild:others

addSubtrie node letter remaining val = 
        let children = getChildren node
            addChild = \child -> setChildren node $ child:children
        in case remaining of
                [] -> addChild $ Node letter [] val
                (x:xs) -> addChild $ addSubtrie (Node letter [] 0) x xs val

lookup :: String -> Trie -> Int
lookup [] (Node _ _ val) = val
lookup (letter:remaining) node =
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
