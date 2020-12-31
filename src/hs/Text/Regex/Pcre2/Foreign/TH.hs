module Text.Regex.Pcre2.Foreign.TH where

import Language.Haskell.TH

{-
    For example,
constant "ERROR_NOMATCH" ''CInt
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
getter "callout_block" "version" ''CUInt
    will produce
foreign import capi unsafe "getters.h" pcre2_callout_block_version
    :: Ptr Pcre2_callout_block
    -> IO CUInt
-}
getter :: String -> TypeQ -> String -> DecsQ
getter blockSuffix fieldTypeQ field = do
    fieldType <- fieldTypeQ
    let dec = ForeignD $ ImportF CApi Unsafe "getters.h" name ty
        name = mkName $ "pcre2_" ++ blockSuffix ++ "_" ++ field
        ty = AppT
            (AppT ArrowT $ AppT
                (ConT $ mkName "Ptr")
                (ConT $ mkName $ "Pcre2_" ++ blockSuffix))
            (AppT
                (ConT $ mkName "IO")
                fieldType)
    return [dec]