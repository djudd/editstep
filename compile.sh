rm *.o *.hi
#ghc -O2 -prof -auto-all -package containers-0.3.0.0 Trie.hs editstep.hs -o editstep
ghc -O2 -package containers-0.3.0.0 Trie.hs editstep.hs -o editstep 
