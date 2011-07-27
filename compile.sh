rm *.o *.hi
ghc -prof -auto -package containers-0.3.0.0 Trie.hs EditStepGraph.hs editstep.hs -o editstep
#ghc -package containers-0.3.0.0 Trie.hs EditStepGraph.hs editstep.hs -o editstep
