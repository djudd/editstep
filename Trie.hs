module Trie 
( empty,
  insert,
  contains,
  permutations,
  longestEditStepLadder
) where

import qualified Data.List as List
import Data.Ord
import Data.Maybe

data Trie = Node Char [Trie] | Leaf Int deriving (Show)

-- utility functions
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

-- insertion
insert :: Int -> String -> Trie -> Trie
insert val [] (Node letter children) =
        let fertileChildren = filter (not . isLeaf) children
        in Node letter (Leaf val:fertileChildren)
insert val (letter:remaining) node = 
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

setChildren (Node letter _) children =
        Node letter children

-- lookup
contains :: String -> Trie -> Bool
contains word trie = (Trie.lookup word trie) > 0

lookup :: String -> Trie -> Int
lookup [] (Leaf _) = 0
lookup [] (Node _ children) =
        case List.find isLeaf children of
                Just (Leaf val) -> val
                Nothing -> 0
lookup (letter:remaining) node =
        case findChild node letter of
                Just child -> Trie.lookup remaining child
                Nothing -> 0

permutations :: String -> Trie -> [String]
permutations word node =
        let params = zip (splits word) (subtries node word)
        in generate additions params $ generate substitutions params $ generate deletions params []

longestEditStepLadder :: [String] -> Int
longestEditStepLadder words = 
        let trie = foldl insertUpdatingLongestPath Trie.empty words
        in maximum $ map ((flip Trie.lookup) trie) words

insertUpdatingLongestPath :: Trie -> String -> Trie
insertUpdatingLongestPath trie word =
        let trie' = insert 1 word trie
            perms = permutations word trie'
            subpathLengths = map ((flip Trie.lookup) trie') perms
            longestSubpath = if null subpathLengths then 0 else maximum subpathLengths 
        in insert (longestSubpath+1) word trie

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
