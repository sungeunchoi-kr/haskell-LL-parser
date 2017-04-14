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
  , sc_identifier   = IDENTIFIER
  , sc_constant     = CONSTANT
  , sc_keyword      = KEYWORD
  }

foreign import ccall "main.h scan" c_scan :: IO (ScannerEnum)
foreign import ccall "main.h load_source" c_load_source :: CString -> IO ()

