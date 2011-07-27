module Trie 
( empty,
  insert,
  contains,
  permutations
) where

import qualified Data.List as List
import Data.Ord
import Data.Maybe

data Trie = Node Char [Trie] | Leaf deriving (Show)

-- utility functions
eow = '\0'
empty = Node eow []

getLetter (Node letter _) = letter
getLetter Leaf = eow

getChildren (Node _ children) = children
getChildren Leaf = []

isLeaf Leaf = True
isLeaf (Node _ _) = False

hasLetter letter Leaf = False
hasLetter letter (Node value _) = value == letter

findChild Leaf letter = Nothing
findChild (Node _ children) letter = {-#SCC "findChild" #-}
        List.find (hasLetter letter) children

hasLeaf Leaf = False
hasLeaf (Node _ children) = 
        List.any isLeaf children

-- insertion
insert :: Trie -> String -> Trie
insert node [] =
        if hasLeaf node 
        then node
        else let letter = getLetter node
                 children = getChildren node
             in Node letter (Leaf:children)
insert node (letter:remaining) = 
        case findChild node letter of
                Just child -> modifyChild node child remaining
                Nothing -> addSubTrie node letter remaining

modifyChild parent child remaining =
        let isOther = \candidate -> (getLetter candidate) /= (getLetter child)
            otherChildren = filter isOther (getChildren parent)
            modifiedChild = insert child remaining
        in setChildren parent (modifiedChild:otherChildren)

addSubTrie node letter remaining = 
        let children = getChildren node
            addChild = \child -> setChildren node $ child:children
        in case remaining of
                [] -> addChild $ Node letter [Leaf]
                (x:xs) -> addChild $ addSubTrie (Node letter []) x xs

setChildren (Node letter _) children =
        Node letter children

-- lookup
contains :: String -> Trie -> Bool
contains [] node = hasLeaf node
contains (letter:remaining) node =
        case findChild node letter of
                Just child -> contains remaining child
                Nothing -> False

permutations :: String -> Trie -> [String]
permutations word node =
        let params = zip (splits word) (subtries node word)
        in generate additions params $ generate substitutions params $ generate deletions params []

generate generator params results = 
        let generator' = \result ((prefix, suffix), node) -> generator result prefix suffix node
        in foldl generator' results params

splits word = [(take n word, drop n word) | n <- [0..(length word)]]

subtries node word = scanl subtrie node word
subtrie node letter = fromJust $ findChild node letter

substitutions result prefix [] node = result
substitutions result prefix (letter:suffix) node = {-#SCC "substitutions" #-}
        let otherChildren = filter (not . (hasLetter letter)) $ getChildren node
            substitutes = lettersForNodesContaining suffix otherChildren
        in [prefix ++ [substitute] ++ suffix | substitute <- substitutes] ++ result

additions result prefix rest node = {-#SCC "additions" #-}
        let addends = lettersForNodesContaining rest $ getChildren node
        in [prefix ++ [addend] ++ rest | addend <- addends] ++ result

lettersForNodesContaining word nodes = {-#SCC "deletions" #-}
        map getLetter $ filter (contains word) nodes

deletions result prefix [] node = result
deletions result prefix (_:suffix) node =
        if contains suffix node
        then (prefix ++ suffix):result 
        else result
