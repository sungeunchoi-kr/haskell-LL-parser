import Data.Map (Map)
import qualified Data.Map as Map

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

----grammarRules = Map Integer GrammarRule
--grammarRules = Map.fromList [(SS,[BLOCK,MORE_BLOCKS])]

main = putStrLn "Hello, World!"

