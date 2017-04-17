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
sc_EPSILON           :: ScannerEnum
sc_EPSILON           = ScannerEnum (-1)
sc_SKIP              :: ScannerEnum
sc_SKIP              = ScannerEnum 0
sc_identifier        :: ScannerEnum
sc_identifier        = ScannerEnum 1
sc_constant          :: ScannerEnum
sc_constant          = ScannerEnum 2
sc_keyword           :: ScannerEnum
sc_keyword           = ScannerEnum 3
sc_KW_class          :: ScannerEnum
sc_KW_class          = ScannerEnum 4095
sc_KW_ssalc          :: ScannerEnum
sc_KW_ssalc          = ScannerEnum 4096
sc_KW_forall         :: ScannerEnum
sc_KW_forall         = ScannerEnum 4097
sc_KW_formyselfonly  :: ScannerEnum
sc_KW_formyselfonly  = ScannerEnum 4098
sc_KW_method         :: ScannerEnum
sc_KW_method         = ScannerEnum 4099
sc_KW_dohtem         :: ScannerEnum
sc_KW_dohtem         = ScannerEnum 4100
sc_KW_number         :: ScannerEnum
sc_KW_number         = ScannerEnum 4101
sc_KW_boolean        :: ScannerEnum
sc_KW_boolean        = ScannerEnum 4102
sc_KW_see            :: ScannerEnum
sc_KW_see            = ScannerEnum 4103
sc_KW_show           :: ScannerEnum
sc_KW_show           = ScannerEnum 4104
sc_KW_if             :: ScannerEnum
sc_KW_if             = ScannerEnum 4105
sc_KW_otherwise      :: ScannerEnum
sc_KW_otherwise      = ScannerEnum 4106
sc_KW_onlyif         :: ScannerEnum
sc_KW_onlyif         = ScannerEnum 4107
sc_KW_loop           :: ScannerEnum
sc_KW_loop           = ScannerEnum 4108
sc_KW_pool           :: ScannerEnum
sc_KW_pool           = ScannerEnum 4109
sc_KW_copy           :: ScannerEnum
sc_KW_copy           = ScannerEnum 4110
sc_KW_into           :: ScannerEnum
sc_KW_into           = ScannerEnum 4111
sc_KW_exit           :: ScannerEnum
sc_KW_exit           = ScannerEnum 4112
sc_KW_ifnot          :: ScannerEnum
sc_KW_ifnot          = ScannerEnum 4113
sc_KW_nono           :: ScannerEnum
sc_KW_nono           = ScannerEnum 4114
sc_KW_notfalse       :: ScannerEnum
sc_KW_notfalse       = ScannerEnum 4115
sc_KW_nottrue        :: ScannerEnum
sc_KW_nottrue        = ScannerEnum 4116
sc_KW_and            :: ScannerEnum
sc_KW_and            = ScannerEnum 4117
sc_KW_or             :: ScannerEnum
sc_KW_or             = ScannerEnum 4118
sc_KW_less           :: ScannerEnum
sc_KW_less           = ScannerEnum 4119
sc_KW_notequal       :: ScannerEnum
sc_KW_notequal       = ScannerEnum 4120
sc_KW_add            :: ScannerEnum
sc_KW_add            = ScannerEnum 4121
sc_SP_LPAREN         :: ScannerEnum
sc_SP_LPAREN         = ScannerEnum 4128
sc_SP_RPAREN         :: ScannerEnum
sc_SP_RPAREN         = ScannerEnum 4129
sc_SP_SEMICOLON      :: ScannerEnum
sc_SP_SEMICOLON      = ScannerEnum 4130
sc_SP_STAR           :: ScannerEnum
sc_SP_STAR           = ScannerEnum 4131

{-# LINE 53 "Scanner.hsc" #-}

foreign import ccall "main.h scan" c_scan :: IO (ScannerEnum)
foreign import ccall "main.h load_source" c_load_source :: CString -> IO ()

load_source :: String -> IO ()
load_source stringFileName = 
    withCString stringFileName $ \stringFileNameC -> do
        Scanner.c_load_source stringFileNameC

