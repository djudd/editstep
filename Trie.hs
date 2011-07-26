module Trie 
( empty,
  insert,
  contains
) where

import qualified Data.List as List
import Data.Ord

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

findChild (Node _ children) letter = {-#SCC "findChild" #-}
        List.find (hasLetter letter) children

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
contains :: Trie -> String -> Bool
contains node [] = hasLeaf node
contains node (letter:remaining) =
        case findChild node letter of
                Just child -> contains child remaining
                Nothing -> False
