{-# LINE 1 "Scanner.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Scanner.hsc" #-}

module Scanner where

import Foreign
import Foreign.C.Types
import Foreign.C.String


{-# LINE 10 "Scanner.hsc" #-}

newtype ScannerEnum = ScannerEnum { unScanner :: CInt }
    deriving (Eq,Show)

-- PCRE compile options
sc_identifier    :: ScannerEnum
sc_identifier    = ScannerEnum 1
sc_constant      :: ScannerEnum
sc_constant      = ScannerEnum 2
sc_keyword       :: ScannerEnum
sc_keyword       = ScannerEnum 3

{-# LINE 20 "Scanner.hsc" #-}

foreign import ccall "main.h scan" c_scan :: IO (ScannerEnum)
foreign import ccall "main.h load_source" c_load_source :: CString -> IO ()

