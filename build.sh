#!/bin/bash
rm ./run
rm *.o
rm *.hi
hsc2hs Scanner.hsc
ghc -o run main.hs Scanner.hs -L. -lscanner 
