#!/bin/bash
hsc2hs Scanner.hsc
ghc -o run main.hs Scanner.hs -L. -lscanner 
