ghc -package containers-0.3.0.0 editstep.hs -o editstep
./editstep +RTS -H256m -K256m -RTS < wordlists/big
