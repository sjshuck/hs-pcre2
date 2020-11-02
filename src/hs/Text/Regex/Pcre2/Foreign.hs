{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A complete binding to the low-level C API.
--
-- Items here are named identically to their C counterparts.  Therefore,
-- documentation will be sparse; the official PCRE2 API docs should suffice.
module Text.Regex.Pcre2.Foreign where

import Foreign
import Foreign.C.Types
import Text.Regex.Pcre2.Foreign.TH (constant, getter)

-- * Types

-- | \"The @UCHAR@ types define unsigned code units of the appropriate widths.
-- For example, @PCRE2_UCHAR16@ is usually defined as @uint16_t@.\"
type PCRE2_UCHAR = CUShort

-- | \"The @SPTR@ types are constant pointers to the equivalent @UCHAR@ types,
-- that is, they are pointers to vectors of unsigned code units.\"
type PCRE2_SPTR = Ptr PCRE2_UCHAR

-- | \"...string lengths and offsets into strings of code units...are always of
-- type @PCRE2_SIZE@...currently always defined as @size_t@.\"
type PCRE2_SIZE = CSize

-- ** Opaque types
--
-- $OpaqueTypes
-- These correspond to structs that are only ever passed by reference.  They
-- simply exist for type safety and cannot be constructed in Haskell.
data Pcre2_general_context
data Pcre2_compile_context
data Pcre2_match_context
data Pcre2_callout_block
data Pcre2_substitute_callout_block
data Pcre2_code
data Pcre2_jit_stack
data Pcre2_match_data
data Pcre2_callout_enumerate_block

-- * The general context

foreign import capi safe "pcre2.h" pcre2_general_context_create
    :: FunPtr (PCRE2_SIZE -> Ptr a -> IO (Ptr b)) -- ^ custom malloc
    -> FunPtr (Ptr b -> Ptr a -> IO ())           -- ^ custom free
    -> Ptr a
    -> IO (Ptr Pcre2_general_context)

foreign import capi safe "pcre2.h" pcre2_general_context_copy
    :: Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_general_context)

foreign import capi safe "pcre2.h" pcre2_general_context_free
    :: Ptr Pcre2_general_context
    -> IO ()

-- * The compile context

foreign import capi safe "pcre2.h" pcre2_compile_context_create
    :: Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_compile_context)

foreign import capi safe "pcre2.h" pcre2_compile_context_copy
    :: Ptr Pcre2_compile_context
    -> IO (Ptr Pcre2_compile_context)

foreign import capi safe "pcre2.h" pcre2_compile_context_free
    :: Ptr Pcre2_compile_context
    -> IO ()

-- ** Compile context setters

-- TODO Maybe this goes next to wherever errors appear in the docs?
constant "ERROR_BADDATA" ''CInt

foreign import capi unsafe "pcre2.h" pcre2_set_bsr
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See below for possible values.
    -> IO CInt

constant "BSR_ANYCRLF" ''CUInt
constant "BSR_UNICODE" ''CUInt

{- FIXME Probably not until 10.35
foreign import capi "pcre2.h" pcre2_set_character_tables
    :: Ptr Pcre2_compile_context
    -> Ptr CUChar
    -> IO CInt
-}

foreign import capi unsafe "pcre2.h" pcre2_set_compile_extra_options
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See \"Extra compile options\" below for possible bit flags.
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_max_pattern_length
    :: Ptr Pcre2_compile_context
    -> PCRE2_SIZE
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_newline
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See below for possible values.
    -> IO CInt

constant "NEWLINE_CR" ''CUInt
constant "NEWLINE_LF" ''CUInt
constant "NEWLINE_CRLF" ''CUInt
constant "NEWLINE_ANY" ''CUInt
constant "NEWLINE_ANYCRLF" ''CUInt
constant "NEWLINE_NUL" ''CUInt

foreign import capi unsafe "pcre2.h" pcre2_set_parens_nest_limit
    :: Ptr Pcre2_compile_context
    -> CUInt
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_compile_recursion_guard
    :: Ptr Pcre2_compile_context
    -> FunPtr (CUInt -> Ptr a -> IO CInt)
    -> Ptr a
    -> IO CInt

-- * The match context

foreign import capi safe "pcre2.h" pcre2_match_context_create
    :: Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_match_context)

foreign import capi safe "pcre2.h" pcre2_match_context_copy
    :: Ptr Pcre2_match_context
    -> IO (Ptr Pcre2_match_context)

foreign import capi safe "pcre2.h" pcre2_match_context_free
    :: Ptr Pcre2_match_context
    -> IO ()

-- ** Match context setters

-- TODO See docs to see how this works
foreign import capi unsafe "pcre2.h" pcre2_set_callout
    :: Ptr Pcre2_match_context
    -> FunPtr (Ptr Pcre2_callout_block -> Ptr a -> IO CInt)
    -> Ptr a
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_substitute_callout
    :: Ptr Pcre2_match_context
    -> FunPtr (Ptr Pcre2_substitute_callout_block -> Ptr a -> IO CInt)
    -> Ptr a
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_offset_limit
    :: Ptr Pcre2_match_context
    -> PCRE2_SIZE -- ^ Can be unset.  See below.
    -> IO CInt

constant "UNSET" ''PCRE2_SIZE

foreign import capi unsafe "pcre2.h" pcre2_set_heap_limit
    :: Ptr Pcre2_match_context
    -> CUInt
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_match_limit
    :: Ptr Pcre2_match_context
    -> CUInt
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_depth_limit
    :: Ptr Pcre2_match_context
    -> CUInt
    -> IO CInt

-- * Checking build-time options

foreign import capi unsafe "pcre2.h" pcre2_config
    :: CUInt   -- ^ See below for possible values.
    -> Ptr a   -- ^ See the C API docs for what gets returned here.
    -> IO CInt

constant "CONFIG_BSR" ''CUInt
constant "CONFIG_COMPILED_WIDTHS" ''CUInt
constant "CONFIG_DEPTHLIMIT" ''CUInt
constant "CONFIG_HEAPLIMIT" ''CUInt
constant "CONFIG_JIT" ''CUInt
constant "CONFIG_JITTARGET" ''CUInt
constant "CONFIG_LINKSIZE" ''CUInt
constant "CONFIG_MATCHLIMIT" ''CUInt
constant "CONFIG_NEWLINE" ''CUInt
constant "CONFIG_NEVER_BACKSLASH_C" ''CUInt
constant "CONFIG_PARENSLIMIT" ''CUInt
constant "CONFIG_STACKRECURSE" ''CUInt
-- constant "CONFIG_TABLES_LENGTH" ''CUInt   10.35+?
constant "CONFIG_UNICODE_VERSION" ''CUInt
constant "CONFIG_UNICODE" ''CUInt
constant "CONFIG_VERSION" ''CUInt

-- * Compiling a pattern

foreign import capi safe "pcre2.h" pcre2_compile
    :: PCRE2_SPTR
    -> PCRE2_SIZE -- ^ Can be zero-terminated.  See below.
    -> CUInt
    -- ^ See \"Main compile options\" below for possible bit flags.
    -> Ptr CInt
    -> Ptr PCRE2_SIZE
    -> Ptr Pcre2_compile_context
    -> IO (Ptr Pcre2_code)

constant "ZERO_TERMINATED" ''PCRE2_SIZE

foreign import capi safe "pcre2.h" pcre2_code_free
    :: Ptr Pcre2_code
    -> IO ()

foreign import capi safe "pcre2.h" pcre2_code_copy
    :: Ptr Pcre2_code
    -> IO (Ptr Pcre2_code)

{- TODO Maybe 10.35+?
foreign import capi "pcre2.h" pcre2_code_copy_with_tables
    :: Ptr Pcre2_code
    -> IO (Ptr Pcre2_code)
-}

-- ** Main compile options
constant "ANCHORED" ''CUInt
constant "ALLOW_EMPTY_CLASS" ''CUInt
constant "ALT_BSUX" ''CUInt
constant "ALT_CIRCUMFLEX" ''CUInt
constant "ALT_VERBNAMES" ''CUInt
constant "AUTO_CALLOUT" ''CUInt
constant "CASELESS" ''CUInt
constant "DOLLAR_ENDONLY" ''CUInt
constant "DOTALL" ''CUInt
constant "DUPNAMES" ''CUInt
constant "ENDANCHORED" ''CUInt
constant "EXTENDED" ''CUInt
constant "EXTENDED_MORE" ''CUInt
constant "FIRSTLINE" ''CUInt
constant "LITERAL" ''CUInt
constant "MATCH_INVALID_UTF" ''CUInt
constant "MATCH_UNSET_BACKREF" ''CUInt
constant "MULTILINE" ''CUInt
constant "NEVER_BACKSLASH_C" ''CUInt
constant "NEVER_UCP" ''CUInt
constant "NEVER_UTF" ''CUInt
constant "NO_AUTO_CAPTURE" ''CUInt
constant "NO_AUTO_POSSESS" ''CUInt
constant "NO_DOTSTAR_ANCHOR" ''CUInt
constant "NO_START_OPTIMIZE" ''CUInt
constant "NO_UTF_CHECK" ''CUInt
constant "UCP" ''CUInt
constant "UNGREEDY" ''CUInt
constant "USE_OFFSET_LIMIT" ''CUInt
constant "UTF" ''CUInt

-- ** Extra compile options
constant "EXTRA_ALLOW_SURROGATE_ESCAPES" ''CUInt
constant "EXTRA_ALT_BSUX" ''CUInt
constant "EXTRA_BAD_ESCAPE_IS_LITERAL" ''CUInt
constant "EXTRA_ESCAPED_CR_IS_LF" ''CUInt
constant "EXTRA_MATCH_LINE" ''CUInt
constant "EXTRA_MATCH_WORD" ''CUInt

-- * JIT compilation

foreign import capi safe "pcre2.h" pcre2_jit_compile
    :: Ptr Pcre2_code
    -> CUInt -- ^ See below for possible bit flags.
    -> IO CInt

constant "JIT_COMPLETE" ''CUInt
constant "JIT_PARTIAL_HARD" ''CUInt
constant "JIT_PARTIAL_SOFT" ''CUInt

-- | May be returned by 'pcre2_jit_compile'.
constant "ERROR_JIT_BADOPTION" ''CInt

foreign import capi safe "pcre2.h" pcre2_jit_match
    :: Ptr Pcre2_code
    -> PCRE2_SPTR              -- ^ subject
    -> PCRE2_SIZE              -- ^ length
    -> PCRE2_SIZE              -- ^ startoffset
    -> CUInt                   -- ^ options
    -> Ptr Pcre2_match_data
    -> Ptr Pcre2_match_context
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_jit_free_unused_memory
    :: Ptr Pcre2_general_context
    -> IO ()

foreign import capi safe "pcre2.h" pcre2_jit_stack_create
    :: PCRE2_SIZE
    -> PCRE2_SIZE
    -> Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_jit_stack)

foreign import capi unsafe "pcre2.h" pcre2_jit_stack_assign
    :: Ptr Pcre2_match_context
    -> FunPtr (Ptr a -> IO (Ptr Pcre2_jit_stack))
    -> Ptr a
    -> IO ()

foreign import capi safe "pcre2.h" pcre2_jit_stack_free
    :: Ptr Pcre2_jit_stack
    -> IO ()

-- * Locale support

foreign import capi safe "pcre2.h" pcre2_maketables
    :: Ptr Pcre2_general_context
    -> IO (Ptr CUChar)

foreign import capi safe "pcre2.h" pcre2_maketables_free
    :: Ptr Pcre2_general_context
    -> Ptr CUChar
    -> IO ()

-- * Information about a compiled pattern

foreign import capi unsafe "pcre2.h" pcre2_pattern_info
    :: Ptr Pcre2_code
    -> CUInt          -- ^ See below for possible values.
    -> Ptr a
    -> IO CInt        -- ^ See below for possible error values.

constant "INFO_ALLOPTIONS" ''CUInt
constant "INFO_ARGOPTIONS" ''CUInt
constant "INFO_EXTRAOPTIONS" ''CUInt
constant "INFO_BACKREFMAX" ''CUInt
constant "INFO_BSR" ''CUInt
constant "INFO_CAPTURECOUNT" ''CUInt
constant "INFO_DEPTHLIMIT" ''CUInt
constant "INFO_FIRSTBITMAP"   ''CUInt
constant "INFO_FIRSTCODETYPE" ''CUInt
constant "INFO_FIRSTCODEUNIT" ''CUInt
constant "INFO_FRAMESIZE" ''CUInt
constant "INFO_HASBACKSLASHC" ''CUInt
constant "INFO_HASCRORLF" ''CUInt
constant "INFO_HEAPLIMIT" ''CUInt
constant "INFO_JCHANGED" ''CUInt
constant "INFO_JITSIZE" ''CUInt
constant "INFO_LASTCODETYPE" ''CUInt
constant "INFO_LASTCODEUNIT" ''CUInt
constant "INFO_MATCHEMPTY" ''CUInt
constant "INFO_MATCHLIMIT" ''CUInt
constant "INFO_MAXLOOKBEHIND" ''CUInt
constant "INFO_MINLENGTH" ''CUInt
constant "INFO_NAMECOUNT" ''CUInt
constant "INFO_NAMEENTRYSIZE" ''CUInt
constant "INFO_NAMETABLE" ''CUInt
constant "INFO_NEWLINE" ''CUInt
constant "INFO_SIZE" ''CUInt

constant "ERROR_UNSET" ''CInt

-- ** Duplicate capture group names
foreign import capi unsafe "pcre2.h" pcre2_substring_nametable_scan
    :: Ptr Pcre2_code
    -> PCRE2_SPTR     -- ^ name
    -> Ptr PCRE2_SPTR -- ^ first
    -> Ptr PCRE2_SPTR -- ^ last
    -> IO CInt

-- * Callouts

foreign import capi safe "pcre2.h" pcre2_callout_enumerate
    :: Ptr Pcre2_code
    -> FunPtr (Ptr Pcre2_callout_enumerate_block -> Ptr a -> IO CInt)
    -> Ptr a
    -> IO CInt

-- ** Callout struct getters
--
-- $CalloutStructGetters
-- These functions are not part of the official API; we provide them here.  C
-- API consumers are supposed to access struct fields directly, but the FFI
-- cannot yet do that as of GHC 8.8.4.

-- *** @pcre2_callout_block@
getter "callout_block" "version" ''CUInt
getter "callout_block" "callout_number" ''CUInt
getter "callout_block" "capture_top" ''CUInt
getter "callout_block" "capture_last" ''CUInt
getter "callout_block" "callout_flags" ''CUInt
-- ^ See below for possible bit flags.
constant "CALLOUT_STARTMATCH" ''CUInt
constant "CALLOUT_BACKTRACK" ''CUInt
foreign import capi unsafe "getters.h" pcre2_callout_block_offset_vector
    :: Ptr Pcre2_callout_block
    -> IO (Ptr PCRE2_SIZE)
getter "callout_block" "mark" ''PCRE2_SPTR
getter "callout_block" "subject" ''PCRE2_SPTR
getter "callout_block" "subject_length" ''PCRE2_SIZE
getter "callout_block" "start_match" ''PCRE2_SIZE
getter "callout_block" "current_position" ''PCRE2_SIZE
getter "callout_block" "pattern_position" ''PCRE2_SIZE
getter "callout_block" "next_item_length" ''PCRE2_SIZE
getter "callout_block" "callout_string_offset" ''PCRE2_SIZE
getter "callout_block" "callout_string_length" ''PCRE2_SIZE
getter "callout_block" "callout_string" ''PCRE2_SPTR

-- *** @pcre2_callout_enumerate_block@
getter "callout_enumerate_block" "version" ''CUInt
getter "callout_enumerate_block" "pattern_position" ''PCRE2_SIZE
getter "callout_enumerate_block" "next_item_length" ''PCRE2_SIZE
getter "callout_enumerate_block" "callout_number" ''CUInt
getter "callout_enumerate_block" "callout_string_offset" ''PCRE2_SIZE
getter "callout_enumerate_block" "callout_string_length" ''PCRE2_SIZE
getter "callout_enumerate_block" "callout_string" ''PCRE2_SPTR

-- *** @pcre2_substitute_callout_block@
getter "substitute_callout_block" "version" ''CUInt
getter "substitute_callout_block" "subscount" ''CUInt
getter "substitute_callout_block" "input" ''PCRE2_SPTR
getter "substitute_callout_block" "output" ''PCRE2_SPTR
foreign import capi unsafe "getters.h" pcre2_substitute_callout_block_ovector
    :: Ptr Pcre2_substitute_callout_block
    -> IO (Ptr PCRE2_SIZE)
getter "substitute_callout_block" "oveccount" ''CUInt
foreign import capi unsafe "getters.h"
    pcre2_substitute_callout_block_output_offsets
        :: Ptr Pcre2_substitute_callout_block
        -> IO (Ptr PCRE2_SIZE) -- ^ Array of 2 (@PCRE2_SIZE[2]@)

-- * The match data block

foreign import capi safe "pcre2.h" pcre2_match_data_create
    :: CUInt                     -- ^ ovecsize
    -> Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_match_data)

foreign import capi safe "pcre2.h" pcre2_match_data_create_from_pattern
    :: Ptr Pcre2_code
    -> Ptr Pcre2_general_context
    -> IO (Ptr Pcre2_match_data)

foreign import capi safe "pcre2.h" pcre2_match_data_free
    :: Ptr Pcre2_match_data
    -> IO ()

-- * Matching a pattern

foreign import capi safe "pcre2.h" pcre2_match
    :: Ptr Pcre2_code
    -> PCRE2_SPTR              -- ^ subject
    -> PCRE2_SIZE              -- ^ length
    -> PCRE2_SIZE              -- ^ startoffset
    -> CUInt                   -- ^ See below for possible bit flags.
    -> Ptr Pcre2_match_data
    -> Ptr Pcre2_match_context
    -> IO CInt

-- ** Match options
constant "COPY_MATCHED_SUBJECT" ''CUInt
constant "NOTBOL" ''CUInt
constant "NOTEOL" ''CUInt
constant "NOTEMPTY" ''CUInt
constant "NOTEMPTY_ATSTART" ''CUInt
constant "NO_JIT" ''CUInt
constant "PARTIAL_HARD" ''CUInt
constant "PARTIAL_SOFT" ''CUInt

-- ** Match errors
constant "ERROR_NOMATCH" ''CInt
constant "ERROR_PARTIAL" ''CInt
constant "ERROR_BADMAGIC" ''CInt
constant "ERROR_BADMODE" ''CInt
constant "ERROR_BADOFFSET" ''CInt
constant "ERROR_BADOPTION" ''CInt
constant "ERROR_BADUTFOFFSET" ''CInt
constant "ERROR_CALLOUT" ''CInt
constant "ERROR_DEPTHLIMIT" ''CInt
constant "ERROR_HEAPLIMIT" ''CInt
constant "ERROR_INTERNAL" ''CInt
constant "ERROR_JIT_STACKLIMIT" ''CInt
constant "ERROR_MATCHLIMIT" ''CInt
constant "ERROR_NOMEMORY" ''CInt
constant "ERROR_NULL" ''CInt
-- *** UTF-16-specific errors
--
-- | \"Missing low surrogate at end of string\"
constant "ERROR_UTF16_ERR1" ''CInt
-- | \"Invalid low surrogate follows high surrogate\"
constant "ERROR_UTF16_ERR2" ''CInt
-- | \"Isolated low surrogate\"
constant "ERROR_UTF16_ERR3" ''CInt

-- * DFA matching

foreign import capi safe "pcre2.h" pcre2_dfa_match
    :: Ptr Pcre2_code
    -> PCRE2_SPTR              -- ^ subject
    -> PCRE2_SIZE              -- ^ length
    -> PCRE2_SIZE              -- ^ startoffset
    -> CUInt                   -- ^ options
    -> Ptr Pcre2_match_data
    -> Ptr Pcre2_match_context
    -> Ptr CInt                -- ^ workspace
    -> PCRE2_SIZE              -- ^ wscount
    -> IO CInt

-- ** DFA matching options
constant "DFA_SHORTEST" ''CUInt
constant "DFA_RESTART" ''CUInt

-- ** DFA matching errors
constant "ERROR_DFA_UFUNC" ''CInt
constant "ERROR_DFA_UITEM" ''CInt
constant "ERROR_DFA_UCOND" ''CInt
constant "ERROR_DFA_UINVALID_UTF" ''CInt
constant "ERROR_DFA_WSSIZE" ''CInt
constant "ERROR_DFA_RECURSE" ''CInt
constant "ERROR_DFA_BADRESTART" ''CInt

-- * Returned string and captured substrings

foreign import capi unsafe "pcre2.h" pcre2_get_ovector_count
    :: Ptr Pcre2_match_data
    -> IO CUInt

foreign import capi unsafe "pcre2.h" pcre2_get_ovector_pointer
    :: Ptr Pcre2_match_data
    -> IO (Ptr PCRE2_SIZE)

foreign import capi unsafe "pcre2.h" pcre2_substring_length_bynumber
    :: Ptr Pcre2_match_data
    -> CUInt                -- ^ number
    -> Ptr PCRE2_SIZE       -- ^ length
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_substring_copy_bynumber
    :: Ptr Pcre2_match_data
    -> CUInt                -- ^ number
    -> Ptr PCRE2_UCHAR      -- ^ buffer
    -> Ptr PCRE2_SIZE       -- ^ bufflen
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_substring_get_bynumber
    :: Ptr Pcre2_match_data
    -> CUInt                 -- ^ name
    -> Ptr (Ptr PCRE2_UCHAR) -- ^ bufferptr
    -> Ptr PCRE2_SIZE        -- ^ bufflen
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_substring_list_get
    :: Ptr Pcre2_match_data
    -> Ptr (Ptr (Ptr PCRE2_UCHAR)) -- ^ listptr
    -> Ptr (Ptr PCRE2_SIZE)        -- ^ lengthsptr
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_substring_list_free
    :: Ptr PCRE2_SPTR
    -> IO ()

foreign import capi unsafe "pcre2.h" pcre2_substring_number_from_name
    :: Ptr Pcre2_code
    -> PCRE2_SPTR     -- ^ name
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_substring_length_byname
    :: Ptr Pcre2_match_data
    -> PCRE2_SPTR           -- ^ name
    -> Ptr PCRE2_SIZE       -- ^ length
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_substring_copy_byname
    :: Ptr Pcre2_match_data
    -> PCRE2_SPTR           -- ^ name
    -> Ptr PCRE2_UCHAR      -- ^ buffer
    -> Ptr PCRE2_SIZE       -- ^ bufflen
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_substring_get_byname
    :: Ptr Pcre2_match_data
    -> PCRE2_SPTR            -- ^ name
    -> Ptr (Ptr PCRE2_UCHAR) -- ^ bufferptr
    -> Ptr PCRE2_SIZE        -- ^ bufflen
    -> IO CInt

-- NOTE Maybe after all other substring functions?
foreign import capi safe "pcre2.h" pcre2_substring_free
    :: Ptr PCRE2_UCHAR
    -> IO ()

constant "ERROR_NOSUBSTRING" ''CInt
constant "ERROR_NOUNIQUESUBSTRING" ''CInt
constant "ERROR_UNAVAILABLE" ''CInt

-- * Creating a new string with substitutions

foreign import capi safe "pcre2.h" pcre2_substitute
    :: Ptr Pcre2_code
    -> PCRE2_SPTR              -- ^ subject
    -> PCRE2_SIZE              -- ^ length
    -> PCRE2_SIZE              -- ^ startoffset
    -> CUInt                   -- ^ options
    -> Ptr Pcre2_match_data
    -> Ptr Pcre2_match_context
    -> PCRE2_SPTR              -- ^ replacement
    -> PCRE2_SIZE              -- ^ rlength
    -> Ptr PCRE2_UCHAR         -- ^ outputbuffer
    -> Ptr PCRE2_SIZE          -- ^ outlengthptr
    -> IO CInt

-- ** Substitution options
-- TODO Commented-out options in version 10.35+?
-- constant "SUBSTITUTE_REPLACEMENT_ONLY" ''CUInt
constant "SUBSTITUTE_GLOBAL" ''CUInt
-- constant "SUBSTITUTE_MATCHED" ''CUInt
constant "SUBSTITUTE_OVERFLOW_LENGTH" ''CUInt
-- constant "SUBSTITUTE_OVERFLOW_LITERAL" ''CUInt
constant "SUBSTITUTE_UNKNOWN_UNSET" ''CUInt
constant "SUBSTITUTE_UNSET_EMPTY" ''CUInt
constant "SUBSTITUTE_EXTENDED" ''CUInt

-- ** Substitution errors
constant "ERROR_BADREPLACEMENT" ''CInt
constant "ERROR_BADREPESCAPE" ''CInt
constant "ERROR_BADSUBSTITUTION" ''CInt
constant "ERROR_BADSUBSPATTERN" ''CInt

-- * Other information about a match

foreign import capi unsafe "pcre2.h" pcre2_get_mark
    :: Ptr Pcre2_match_data
    -> IO PCRE2_SPTR

foreign import capi unsafe "pcre2.h" pcre2_get_startchar
    :: Ptr Pcre2_match_data
    -> IO PCRE2_SIZE

-- * Obtaining a textual error message
foreign import capi safe "pcre2.h" pcre2_get_error_message
    :: CInt
    -> Ptr PCRE2_UCHAR
    -> PCRE2_SIZE
    -> IO CInt
