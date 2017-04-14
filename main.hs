{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as ByteString
import qualified Scanner

data Nonterminal = SS | BLOCK | MORE_BLOCKS deriving (Enum, Eq, Ord)
data Terminal = CLASS | METHOD | EPSILON deriving (Enum, Eq, Ord)
data Symbol = T Terminal | N Nonterminal
data Rule = Rule Nonterminal [Symbol]

--constructGrammarTable :: Nonterminal -> Symbol --Either Terminal Nonterminal
--constructGrammarTable n
--    | n == SS                       = N BLOCK
--    | n == BLOCK                    = T EPSILON
--    | n == MORE_BLOCKS              = T EPSILON

--gammarTable :: [Rule]
--gammarTable = [(Rule SS $ N BLOCK)]

indexedGrammarTable :: Map Integer Rule
indexedGrammarTable = Map.fromList
    [
        (1, Rule SS             $ [(N BLOCK), (N MORE_BLOCKS)]),
        (2, Rule MORE_BLOCKS    $ [T EPSILON]),
        (3, Rule MORE_BLOCKS    $ [(N BLOCK), (N MORE_BLOCKS)])
    ]

parseTable :: Map Terminal Int 
parseTable = Map.fromList
    [
        (CLASS, 1)
    ]

--main = putStrLn "Hello, World!"

cc_load_source :: String -> IO ()
cc_load_source stringFileName = 
  withCString stringFileName $ \stringFileNameC -> do
        Scanner.c_load_source stringFileNameC

main = do
    putStrLn "Hello, World!"
    cc_load_source "source.ss"
    token <- Scanner.c_scan
    print token
