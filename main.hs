{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Map (Map)
import qualified Data.Map as Map
--import Control.Conditional
--import Control.Monad
import qualified Scanner

--(==:) :: ( Eq a,Monad m) => m a -> m a -> m Bool
--(==:) = liftM2 (==)

--main = ifM (getLine ==: getLine) (print "hit") (print "miss")

data Nonterminal = N_SS | N_BLOCK | N_MORE_BLOCKS | N_CLASS | N_METHOD | N_MODIFIER |
        N_ID_LIST | N_MORE_IDS | N_METHODS | N_MORE_METHODS | N_STMTS | N_STMT | N_MORE_STMTS |
        N_DECL | N_INPUT | N_OUTPUT | N_COND | N_LOOP | N_ASMT | N_EXIT | N_TYPE |
        N_EXPR | N_BOOL_EXPR | N_ARITH_EXPR | N_BOOL_TAIL | N_TEST deriving (Enum, Eq, Ord, Show)

data Terminal = IDENTIFIER| CONSTANT |  CLASS | SSALC | FORALL | FORMYSELFONLY | METHOD | DOHTEM | NUMBER |
        BOOLEAN | SEE | SHOW | IF | OTHERWISE | ONLYIF | LOOP | POOL | COPY | INTO |
        EXIT | IFNOT | NONO | NOTFALSE | NOTTRUE | AND | OR | LESS | NOTEQUAL | ADD |
        LPAREN | RPAREN | SEMICOLON | STAR | EPSILON
            deriving (Enum, Eq, Ord, Show)

data Symbol = Terminal Terminal | Nonterminal Nonterminal
data Rule = Rule Nonterminal [Symbol]

--constructGrammarTable :: Nonterminal -> Symbol --Either Terminal Nonterminal
--constructGrammarTable n
--    | n == SS                       = N BLOCK
--    | n == BLOCK                    = T EPSILON
--    | n == MORE_BLOCKS              = T EPSILON

--gammarTable :: [Rule]
--gammarTable = [(Rule SS $ N BLOCK)]

indexedCFGRules :: Map Int (Nonterminal,[Symbol])
indexedCFGRules = Map.fromList(
    [
         (1, (N_SS           , [(Nonterminal N_BLOCK), (Nonterminal N_MORE_BLOCKS)]))
        ,(2, (N_MORE_BLOCKS  , [Terminal EPSILON]))
        ,(3, (N_MORE_BLOCKS  , [(Nonterminal N_BLOCK), (Nonterminal N_MORE_BLOCKS)]))
    ])

parseTable :: Map (Nonterminal, Terminal) Int
parseTable = Map.fromList(
    [
         ((N_SS            , CLASS        )      ,  1)
        ,((N_SS            , METHOD       )      ,  1)

        ,((N_MORE_BLOCKS   , EPSILON      )      ,  2)
        ,((N_MORE_BLOCKS   , CLASS        )      ,  3)
        ,((N_MORE_BLOCKS   , METHOD       )      ,  3)

        ,((N_BLOCK         , CLASS        )      ,  4)
        ,((N_BLOCK         , METHOD       )      ,  5)

        ,((N_CLASS         , CLASS        )      ,  6)

        ,((N_MODIFIER      , FORALL       )      ,  7)
        ,((N_MODIFIER      , FORMYSELFONLY)      ,  8)

        ,((N_ID_LIST       , IDENTIFIER   )      ,  9)

        ,((N_MORE_IDS      , RPAREN       )      , 10)
        ,((N_MORE_IDS      , SEMICOLON    )      , 10)
        ,((N_MORE_IDS      , IDENTIFIER   )      , 11)
    ])

cc_load_source :: String -> IO ()
cc_load_source stringFileName = 
  withCString stringFileName $ \stringFileNameC -> do
        Scanner.c_load_source stringFileNameC

scannerEnumToSymbol :: Scanner.ScannerEnum -> Terminal
scannerEnumToSymbol v
    | v == Scanner.sc_identifier        = IDENTIFIER
    | v == Scanner.sc_constant          = CONSTANT
    | v == Scanner.sc_KW_class          = CLASS
    | v == Scanner.sc_KW_ssalc          = SSALC 
    | v == Scanner.sc_KW_forall         = FORALL 
    | v == Scanner.sc_KW_formyselfonly  = FORMYSELFONLY 
    | v == Scanner.sc_KW_method         = METHOD 
    | v == Scanner.sc_KW_dohtem         = DOHTEM 
    | v == Scanner.sc_KW_number         = NUMBER 
    | v == Scanner.sc_KW_boolean        = BOOLEAN 
    | v == Scanner.sc_KW_see            = SEE 
    | v == Scanner.sc_KW_show           = SHOW 
    | v == Scanner.sc_KW_if             = IF 
    | v == Scanner.sc_KW_otherwise      = OTHERWISE 
    | v == Scanner.sc_KW_onlyif         = ONLYIF 
    | v == Scanner.sc_KW_loop           = LOOP 
    | v == Scanner.sc_KW_pool           = POOL 
    | v == Scanner.sc_KW_copy           = COPY 
    | v == Scanner.sc_KW_into           = INTO 
    | v == Scanner.sc_KW_exit           = EXIT 
    | v == Scanner.sc_KW_ifnot          = IFNOT 
    | v == Scanner.sc_KW_nono           = NONO 
    | v == Scanner.sc_KW_notfalse       = NOTFALSE 
    | v == Scanner.sc_KW_nottrue        = NOTTRUE 
    | v == Scanner.sc_KW_and            = AND 
    | v == Scanner.sc_KW_or             = OR 
    | v == Scanner.sc_KW_less           = LESS 
    | v == Scanner.sc_KW_notequal       = NOTEQUAL 
    | v == Scanner.sc_KW_add            = ADD 
    | v == Scanner.sc_SP_LPAREN         = LPAREN
    | v == Scanner.sc_SP_RPAREN         = RPAREN
    | v == Scanner.sc_SP_SEMICOLON      = SEMICOLON
    | v == Scanner.sc_SP_STAR           = STAR 

compareSymbols :: Symbol -> Symbol -> Bool
compareSymbols (Terminal a) (Terminal b) = a == b
compareSymbols (Nonterminal a) (Nonterminal b) = a == b
compareSymbols _ _ = False

runParser :: [Symbol] -> Maybe Terminal -> IO ()
runParser (x:xs) Nothing = do
    t <- Scanner.c_scan
    lookahead <- return (scannerEnumToSymbol t)
    --print lookahead
    --print x
    case x of
        Nonterminal nonterminalStackTop -> do
            index <- return $ Map.findWithDefault 0 (nonterminalStackTop, lookahead) parseTable
            lookupResult <- return $ Map.lookup index indexedCFGRules
            case lookupResult of
                Nothing -> do 
                    putStrLn "Error. indexedCFGRules returned Nothing."
                    return()
                Just stackTopReplacementRule -> do
                    runParser (snd stackTopReplacementRule ++ xs) Nothing
        Terminal terminalStackTop -> do
            -- just pop for now.
            runParser xs Nothing

runParser (x:xs) (Just lookahead) = do
    runParser xs Nothing

runParser [] _ = do
    return ()

main = do
    putStrLn "Hello, World!"
    cc_load_source "source.ss"
    --val <- return (scannerEnumToSymbol token)
    --comp <- return (compareSymbols val (T CLASS))
    --if token == Scanner.sc_KW_class
    --    then putStrLn "hi"
    --    else putStrLn "hii"
    --if comp == True
    --    then putStrLn "1hi"
    --    else putStrLn "1hii"
    runParser [(Nonterminal N_SS), (Nonterminal N_SS)] Nothing

