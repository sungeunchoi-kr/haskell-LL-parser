{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Scanner where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "scanner.h"

newtype ScannerEnum = ScannerEnum { unScanner :: CInt }
    deriving (Eq,Show)

-- PCRE compile options
#{enum ScannerEnum, ScannerEnum
  , sc_EPSILON          = EPSILON
  , sc_SKIP             = SKIP
  , sc_identifier       = IDENTIFIER
  , sc_constant         = CONSTANT
  , sc_keyword          = KEYWORD
  , sc_KW_class         = KW_class 
  , sc_KW_ssalc         = KW_ssalc 
  , sc_KW_forall        = KW_forall 
  , sc_KW_formyselfonly = KW_formyselfonly 
  , sc_KW_method        = KW_method 
  , sc_KW_dohtem        = KW_dohtem 
  , sc_KW_number        = KW_number 
  , sc_KW_boolean       = KW_boolean 
  , sc_KW_see           = KW_see 
  , sc_KW_show          = KW_show 
  , sc_KW_if            = KW_if 
  , sc_KW_otherwise     = KW_otherwise 
  , sc_KW_onlyif        = KW_onlyif 
  , sc_KW_loop          = KW_loop 
  , sc_KW_pool          = KW_pool 
  , sc_KW_copy          = KW_copy 
  , sc_KW_into          = KW_into 
  , sc_KW_exit          = KW_exit 
  , sc_KW_ifnot         = KW_ifnot 
  , sc_KW_nono          = KW_nono 
  , sc_KW_notfalse      = KW_notfalse 
  , sc_KW_nottrue       = KW_nottrue 
  , sc_KW_and           = KW_and 
  , sc_KW_or            = KW_or 
  , sc_KW_less          = KW_less 
  , sc_KW_notequal      = KW_notequal 
  , sc_KW_add           = KW_add 
  , sc_SP_LPAREN        = SP_LPAREN 
  , sc_SP_RPAREN        = SP_RPAREN
  , sc_SP_SEMICOLON     = SP_SEMICOLON
  , sc_SP_STAR          = SP_STAR 
  }

foreign import ccall "main.h load_source" c_load_source :: CString -> IO ()
foreign import ccall "main.h scan" c_scan :: IO (ScannerEnum)
foreign import ccall "main.h print_symb_table" c_print_symb_table :: IO ()

load_source :: String -> IO ()
load_source stringFileName = 
    withCString stringFileName $ \stringFileNameC -> do
        Scanner.c_load_source stringFileNameC

