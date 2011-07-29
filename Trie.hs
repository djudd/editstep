module Trie 
( insert,
  Trie.lookup,
  empty,
  getLetter,
  getChildren,
  hasLetter,
  findChild,
  otherChildren,
  setChildren,
  Trie(Node)
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

-- trie operations
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
