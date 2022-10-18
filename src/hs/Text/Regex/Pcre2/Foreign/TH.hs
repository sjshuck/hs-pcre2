{-# LANGUAGE TemplateHaskellQuotes #-}

module Text.Regex.Pcre2.Foreign.TH where

import Foreign             (Ptr)
import Language.Haskell.TH

{-
    For example,
constant ''CInt "ERROR_NOMATCH"
    will produce
foreign import capi unsafe "pcre2.h value PCRE2_ERROR_NOMATCH"
    pcre2_ERROR_NOMATCH :: CInt
-}
constant :: Name -> String -> DecsQ
constant typeName suffix = return [dec] where
    dec = ForeignD $ ImportF CApi Unsafe str name ty
    str = "pcre2.h value PCRE2_" ++ suffix
    name = mkName $ "pcre2_" ++ suffix
    ty = ConT typeName

{-
    For example,
getter "callout_block" [t| CUInt |] "version"
    will produce
foreign import capi unsafe "getters.h" pcre2_callout_block_version
    :: Ptr Pcre2_callout_block
    -> IO CUInt
-}
getter :: String -> TypeQ -> String -> DecsQ
getter blockSuffix fieldTypeQ field = do
    let blockName = mkName $ "Pcre2_" ++ blockSuffix
    ty <- [t| Ptr $(conT blockName) -> IO $(fieldTypeQ) |]
    let dec = ForeignD $ ImportF CApi Unsafe "getters.h" name ty
        name = mkName $ "pcre2_" ++ blockSuffix ++ "_" ++ field
    return [dec]
