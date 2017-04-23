import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Scanner

data Nonterminal = N_SS | N_BLOCK | N_MORE_BLOCKS | N_CLASS | N_METHOD | N_MODIFIER |
        N_ID_LIST | N_MORE_IDS | N_METHODS | N_MORE_METHODS | N_STMTS | N_STMT | N_MORE_STMTS |
        N_DECL | N_INPUT | N_OUTPUT | N_COND | N_LOOP | N_ASMT | N_EXIT | N_TYPE |
        N_EXPR | N_BOOL_EXPR | N_ARITH_EXPR | N_BOOL_TAIL | N_TEST deriving (Enum, Eq, Ord, Show)

data Terminal = IDENTIFIER| CONSTANT |  CLASS | SSALC | FORALL | FORMYSELFONLY | METHOD | DOHTEM | NUMBER |
        BOOLEAN | SEE | SHOW | IF | OTHERWISE | ONLYIF | LOOP | POOL | COPY | INTO |
        EXIT | IFNOT | NONO | NOTFALSE | NOTTRUE | AND | OR | LESS | NOTEQUAL | ADD |
        LPAREN | RPAREN | SEMICOLON | STAR | EPSILON | SKIP
            deriving (Enum, Eq, Ord, Show)

data Symbol = Terminal Terminal | Nonterminal Nonterminal deriving (Show)

indexedCFGRules :: Map Int (Nonterminal,[Symbol])
indexedCFGRules = Map.fromList(
    [
         ( 1, (N_SS           , [Nonterminal N_BLOCK, Nonterminal N_MORE_BLOCKS]))
        ,( 2, (N_MORE_BLOCKS  , [Terminal EPSILON]))
        ,( 3, (N_MORE_BLOCKS  , [Nonterminal N_BLOCK, Nonterminal N_MORE_BLOCKS]))
        ,( 4, (N_BLOCK        , [Nonterminal N_CLASS]))
        ,( 5, (N_BLOCK        , [Nonterminal N_METHOD]))
        ,( 6, (N_CLASS        , [(Terminal CLASS), (Terminal IDENTIFIER), (Nonterminal N_MODIFIER), (Terminal LPAREN), (Nonterminal N_ID_LIST), (Terminal RPAREN), (Nonterminal N_METHODS), (Terminal SSALC)]))
        ,( 7, (N_MODIFIER     , [Terminal FORALL]))
        ,( 8, (N_MODIFIER     , [Terminal FORMYSELFONLY]))
        ,( 9, (N_ID_LIST      , [Terminal IDENTIFIER, Nonterminal N_MORE_IDS]))
        ,(10, (N_MORE_IDS     , [Terminal EPSILON]))
        ,(11, (N_MORE_IDS     , [Terminal IDENTIFIER, Nonterminal N_MORE_IDS]))
        ,(12, (N_METHODS      , [Nonterminal N_METHOD, Nonterminal N_MORE_METHODS]))
        ,(13, (N_MORE_METHODS , [Terminal EPSILON]))
        ,(14, (N_MORE_METHODS , [Nonterminal N_METHOD, Nonterminal N_MORE_METHODS]))
        ,(15, (N_METHOD       , [Terminal METHOD, Terminal IDENTIFIER, Nonterminal N_STMTS, Terminal DOHTEM]))
        ,(16, (N_STMTS        , [Nonterminal N_STMT, Terminal SEMICOLON, Nonterminal N_MORE_STMTS]))
        ,(17, (N_MORE_STMTS   , [Terminal EPSILON]))
        ,(18, (N_MORE_STMTS   , [Nonterminal N_STMT, Terminal SEMICOLON, Nonterminal N_MORE_STMTS]))
        ,(19, (N_STMT         , [Nonterminal N_DECL]))
        ,(20, (N_STMT         , [Nonterminal N_INPUT]))
        ,(21, (N_STMT         , [Nonterminal N_OUTPUT]))
        ,(22, (N_STMT         , [Nonterminal N_COND]))
        ,(23, (N_STMT         , [Nonterminal N_LOOP]))
        ,(24, (N_STMT         , [Nonterminal N_ASMT]))
        ,(25, (N_STMT         , [Nonterminal N_EXIT]))
        ,(26, (N_DECL         , [Nonterminal N_TYPE, Nonterminal N_ID_LIST]))
        ,(27, (N_TYPE         , [Terminal NUMBER]))
        ,(28, (N_TYPE         , [Terminal BOOLEAN]))
        ,(29, (N_INPUT        , [Terminal SEE, Nonterminal N_ID_LIST]))
        ,(30, (N_OUTPUT       , [Terminal SHOW, Nonterminal N_ID_LIST]))
        ,(31, (N_COND         , [Terminal IF, Nonterminal N_BOOL_EXPR, Nonterminal N_STMTS, Terminal OTHERWISE, Nonterminal N_STMTS, Terminal ONLYIF, Nonterminal N_BOOL_EXPR]))
        ,(32, (N_LOOP         , [Terminal LOOP, Nonterminal N_STMTS, Terminal POOL]))
        ,(33, (N_ASMT         , [Terminal COPY, Nonterminal N_EXPR, Terminal INTO, Nonterminal N_ID_LIST]))
        ,(34, (N_EXIT         , [Terminal EXIT, Terminal IFNOT, Nonterminal N_BOOL_EXPR]))
        ,(35, (N_EXPR         , [Nonterminal N_BOOL_EXPR]))
        ,(36, (N_EXPR         , [Nonterminal N_ARITH_EXPR]))
        ,(37, (N_BOOL_EXPR    , [Terminal NONO, Nonterminal N_BOOL_EXPR]))
        ,(38, (N_BOOL_EXPR    , [Terminal NOTFALSE, Nonterminal N_BOOL_TAIL]))
        ,(39, (N_BOOL_EXPR    , [Terminal NOTTRUE, Nonterminal N_BOOL_TAIL]))
        ,(40, (N_BOOL_EXPR    , [Nonterminal N_TEST, Nonterminal N_ARITH_EXPR, Nonterminal N_ARITH_EXPR]))
        ,(41, (N_BOOL_TAIL    , [Terminal EPSILON]))
        ,(42, (N_BOOL_TAIL    , [Terminal AND, Nonterminal N_BOOL_EXPR]))
        ,(43, (N_BOOL_TAIL    , [Terminal OR, Nonterminal N_BOOL_EXPR]))
        ,(44, (N_TEST         , [Terminal LESS]))
        ,(45, (N_TEST         , [Terminal NOTEQUAL]))
        ,(46, (N_ARITH_EXPR   , [Terminal ADD, Nonterminal N_ARITH_EXPR, Nonterminal N_ARITH_EXPR]))
        ,(47, (N_ARITH_EXPR   , [Terminal STAR, Nonterminal N_ARITH_EXPR, Nonterminal N_ARITH_EXPR]))
        ,(48, (N_ARITH_EXPR   , [Terminal LPAREN, Terminal IDENTIFIER, Terminal RPAREN]))
        ,(49, (N_ARITH_EXPR   , [Terminal IDENTIFIER]))
        ,(50, (N_ARITH_EXPR   , [Terminal CONSTANT]))
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

        ,((N_METHODS       , METHOD       )      , 12)

        ,((N_MORE_METHODS  , SSALC        )      , 13)
        ,((N_MORE_METHODS  , METHOD       )      , 14)

        ,((N_METHOD        , METHOD       )      , 15)

        ,((N_STMTS         , NUMBER       )      , 16)
        ,((N_STMTS         , BOOLEAN      )      , 16)
        ,((N_STMTS         , SEE          )      , 16)
        ,((N_STMTS         , SHOW         )      , 16)
        ,((N_STMTS         , IF           )      , 16)
        ,((N_STMTS         , LOOP         )      , 16)
        ,((N_STMTS         , COPY         )      , 16)
        ,((N_STMTS         , EXIT         )      , 16)

        ,((N_MORE_STMTS    , DOHTEM       )      , 17)
        ,((N_MORE_STMTS    , OTHERWISE    )      , 17)
        ,((N_MORE_STMTS    , ONLYIF       )      , 17)
        ,((N_MORE_STMTS    , POOL         )      , 17)

        ,((N_MORE_STMTS    , NUMBER       )      , 18)
        ,((N_MORE_STMTS    , BOOLEAN      )      , 18)
        ,((N_MORE_STMTS    , SEE          )      , 18)
        ,((N_MORE_STMTS    , SHOW         )      , 18)
        ,((N_MORE_STMTS    , IF           )      , 18)
        ,((N_MORE_STMTS    , LOOP         )      , 18)
        ,((N_MORE_STMTS    , COPY         )      , 18)
        ,((N_MORE_STMTS    , EXIT         )      , 18)

        ,((N_STMT          , NUMBER       )      , 19)
        ,((N_STMT          , BOOLEAN      )      , 19)
        ,((N_STMT          , SEE          )      , 20)
        ,((N_STMT          , SHOW         )      , 21)
        ,((N_STMT          , IF           )      , 22)
        ,((N_STMT          , LOOP         )      , 23)
        ,((N_STMT          , COPY         )      , 24)
        ,((N_STMT          , EXIT         )      , 25)

        ,((N_DECL          , NUMBER       )      , 26)
        ,((N_DECL          , BOOLEAN      )      , 26)

        ,((N_TYPE          , NUMBER       )      , 27)
        ,((N_TYPE          , BOOLEAN      )      , 28)

        ,((N_INPUT         , SEE          )      , 29)
        ,((N_OUTPUT        , SHOW         )      , 30)
        ,((N_COND          , IF           )      , 31)
        ,((N_LOOP          , LOOP         )      , 32)
        ,((N_ASMT          , COPY         )      , 33)
        ,((N_EXIT          , EXIT         )      , 34)

        ,((N_EXPR          , NONO         )      , 35)
        ,((N_EXPR          , NOTFALSE     )      , 35)
        ,((N_EXPR          , NOTTRUE      )      , 35)
        ,((N_EXPR          , LESS         )      , 35)
        ,((N_EXPR          , NOTEQUAL     )      , 35)
        ,((N_EXPR          , ADD          )      , 36)
        ,((N_EXPR          , STAR         )      , 36)
        ,((N_EXPR          , LPAREN       )      , 36)
        ,((N_EXPR          , IDENTIFIER   )      , 36)
        ,((N_EXPR          , CONSTANT     )      , 36)

        ,((N_BOOL_EXPR     , NONO         )      , 37)
        ,((N_BOOL_EXPR     , NOTFALSE     )      , 38)
        ,((N_BOOL_EXPR     , NOTTRUE      )      , 39)
        ,((N_BOOL_EXPR     , LESS         )      , 40)
        ,((N_BOOL_EXPR     , NOTEQUAL     )      , 40)

        ,((N_BOOL_TAIL     , NUMBER       )      , 41)
        ,((N_BOOL_TAIL     , BOOLEAN      )      , 41)
        ,((N_BOOL_TAIL     , SEE          )      , 41)
        ,((N_BOOL_TAIL     , SHOW         )      , 41)
        ,((N_BOOL_TAIL     , IF           )      , 41)
        ,((N_BOOL_TAIL     , LOOP         )      , 41)
        ,((N_BOOL_TAIL     , COPY         )      , 41)
        ,((N_BOOL_TAIL     , EXIT         )      , 41)
        ,((N_BOOL_TAIL     , SEMICOLON    )      , 41)
        ,((N_BOOL_TAIL     , INTO         )      , 41)

        ,((N_BOOL_TAIL     , AND          )      , 42)
        ,((N_BOOL_TAIL     , OR           )      , 43)

        ,((N_TEST          , LESS         )      , 44)
        ,((N_TEST          , NOTEQUAL     )      , 45)

        ,((N_ARITH_EXPR    , ADD          )      , 46)
        ,((N_ARITH_EXPR    , STAR         )      , 47)
        ,((N_ARITH_EXPR    , LPAREN       )      , 48)
        ,((N_ARITH_EXPR    , IDENTIFIER   )      , 49)
        ,((N_ARITH_EXPR    , CONSTANT     )      , 50)
    ])

scannerEnumToSymbol :: Scanner.ScannerEnum -> Maybe Terminal
scannerEnumToSymbol v
    | v == Scanner.sc_EPSILON           = Just EPSILON
    | v == Scanner.sc_SKIP              = Just SKIP
    | v == Scanner.sc_identifier        = Just IDENTIFIER
    | v == Scanner.sc_constant          = Just CONSTANT
    | v == Scanner.sc_KW_class          = Just CLASS
    | v == Scanner.sc_KW_ssalc          = Just SSALC 
    | v == Scanner.sc_KW_forall         = Just FORALL 
    | v == Scanner.sc_KW_formyselfonly  = Just FORMYSELFONLY 
    | v == Scanner.sc_KW_method         = Just METHOD 
    | v == Scanner.sc_KW_dohtem         = Just DOHTEM 
    | v == Scanner.sc_KW_number         = Just NUMBER 
    | v == Scanner.sc_KW_boolean        = Just BOOLEAN 
    | v == Scanner.sc_KW_see            = Just SEE 
    | v == Scanner.sc_KW_show           = Just SHOW 
    | v == Scanner.sc_KW_if             = Just IF 
    | v == Scanner.sc_KW_otherwise      = Just OTHERWISE 
    | v == Scanner.sc_KW_onlyif         = Just ONLYIF 
    | v == Scanner.sc_KW_loop           = Just LOOP 
    | v == Scanner.sc_KW_pool           = Just POOL 
    | v == Scanner.sc_KW_copy           = Just COPY 
    | v == Scanner.sc_KW_into           = Just INTO 
    | v == Scanner.sc_KW_exit           = Just EXIT 
    | v == Scanner.sc_KW_ifnot          = Just IFNOT 
    | v == Scanner.sc_KW_nono           = Just NONO 
    | v == Scanner.sc_KW_notfalse       = Just NOTFALSE 
    | v == Scanner.sc_KW_nottrue        = Just NOTTRUE 
    | v == Scanner.sc_KW_and            = Just AND 
    | v == Scanner.sc_KW_or             = Just OR 
    | v == Scanner.sc_KW_less           = Just LESS 
    | v == Scanner.sc_KW_notequal       = Just NOTEQUAL 
    | v == Scanner.sc_KW_add            = Just ADD 
    | v == Scanner.sc_SP_LPAREN         = Just LPAREN
    | v == Scanner.sc_SP_RPAREN         = Just RPAREN
    | v == Scanner.sc_SP_SEMICOLON      = Just SEMICOLON
    | v == Scanner.sc_SP_STAR           = Just STAR 
scannerEnumToSymbol _ = Nothing

compareSymbols :: Symbol -> Symbol -> Bool
compareSymbols (Terminal a) (Terminal b) = a == b
compareSymbols (Nonterminal a) (Nonterminal b) = a == b
compareSymbols _ _ = False

printCurrentState :: Symbol -> Terminal -> IO()
printCurrentState t l = do
    putStr "stack top:"
    putStr . show $ t
    putStr " |  lookahead:" 
    putStr . show $ l
    putStrLn ""

-- parameters:
--     top of stack
--     rest of the stack
--     lookahead
runParserHelper :: Symbol -> [Symbol] -> Terminal -> IO()
runParserHelper (Nonterminal nonterminalStackTop) xs lookahead = do
    printCurrentState (Nonterminal nonterminalStackTop) lookahead
    index <- return $ Map.findWithDefault 0 (nonterminalStackTop, lookahead) parseTable
    lookupResult <- return $ Map.lookup index indexedCFGRules
    case lookupResult of
        Nothing -> do 
            putStrLn "a) Error. indexedCFGRules returned Nothing."
            return()
        Just stackTopReplacementRule -> do
            runParser ((snd stackTopReplacementRule) ++ xs) (Just lookahead)

runParserHelper (Terminal EPSILON) xs lookahead = do
    runParser xs (Just lookahead)

runParserHelper (Terminal terminalStackTop) xs lookahead = do
    printCurrentState (Terminal terminalStackTop) lookahead
    compresult <- return $ compareSymbols (Terminal terminalStackTop) (Terminal lookahead)
    if compresult == True then
        runParser xs Nothing
    else
        putStrLn "POP MISMATCH"


runParser :: [Symbol] -> Maybe Terminal -> IO ()
runParser (x:xs) Nothing = do
    t <- Scanner.c_scan
    lookahead <- return (scannerEnumToSymbol t)
    case lookahead of
        Nothing -> do
            putStrLn "Fatal Error."
            return ()
        _ -> do
             runParser (x:xs) (lookahead)

runParser (x:xs) (Just lookahead) = do
    printCurrentState x lookahead
    runParserHelper x xs lookahead

runParser [] (Just EPSILON) = do
    putStrLn "Encountered stack bottom with correct conditions."
    return ()

runParser [] t = do
    putStr "Error: Encountered stack bottom but lookahead is not EPSILON, is "
    print t
    return ()

main = do
    Scanner.load_source "source.ss"
    runParser [(Nonterminal N_SS)] Nothing

