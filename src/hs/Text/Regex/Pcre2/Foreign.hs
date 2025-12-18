{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A complete binding to the low-level C API.
--
-- Items here are named identically to their C counterparts.  Therefore,
-- documentation will be sparse;
-- [the official PCRE2 API docs](https://pcre.org/current/doc/html/pcre2api.html)
-- should suffice.
module Text.Regex.Pcre2.Foreign where

import Foreign
import Foreign.C.Types
import Text.Regex.Pcre2.Foreign.TH (constant, getter)

-- * Types

-- | "The @UCHAR@ types define unsigned code units of the appropriate widths.
-- For example, @PCRE2_UCHAR16@ is usually defined as @uint16_t@."
type PCRE2_UCHAR = CUChar

-- | "The @SPTR@ types are constant pointers to the equivalent @UCHAR@ types,
-- that is, they are pointers to vectors of unsigned code units."
type PCRE2_SPTR = Ptr PCRE2_UCHAR

-- | "...string lengths and offsets into strings of code units...are always of
-- type @PCRE2_SIZE@...currently always defined as @size_t@."
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

foreign import capi safe "pcre2.h &pcre2_general_context_free_8"
    pcre2_general_context_finalizer :: FinalizerPtr Pcre2_general_context

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

foreign import capi safe "pcre2.h &pcre2_compile_context_free_8"
    pcre2_compile_context_finalizer :: FinalizerPtr Pcre2_compile_context

-- ** Compile context setters

-- TODO Maybe this goes next to wherever errors appear in the docs?
constant ''CInt "ERROR_BADDATA"

foreign import capi unsafe "pcre2.h" pcre2_set_bsr
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See below for possible values.
    -> IO CInt

constant ''CUInt "BSR_ANYCRLF"
constant ''CUInt "BSR_UNICODE"

foreign import capi unsafe "pcre2.h" pcre2_set_character_tables
    :: Ptr Pcre2_compile_context
    -> Ptr CUChar
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_compile_extra_options
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See "Extra compile options" below for possible bit flags.
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_max_pattern_length
    :: Ptr Pcre2_compile_context
    -> PCRE2_SIZE
    -> IO CInt

foreign import capi unsafe "pcre2.h" pcre2_set_newline
    :: Ptr Pcre2_compile_context
    -> CUInt -- ^ See below for possible values.
    -> IO CInt

constant ''CUInt "NEWLINE_CR"
constant ''CUInt "NEWLINE_LF"
constant ''CUInt "NEWLINE_CRLF"
constant ''CUInt "NEWLINE_ANY"
constant ''CUInt "NEWLINE_ANYCRLF"
constant ''CUInt "NEWLINE_NUL"

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

foreign import capi safe "pcre2.h &pcre2_match_context_free_8"
    pcre2_match_context_finalizer :: FinalizerPtr Pcre2_match_context

-- ** Match context setters

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

constant ''PCRE2_SIZE "UNSET"

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

constant ''CUInt "CONFIG_BSR"
constant ''CUInt "CONFIG_COMPILED_WIDTHS"
constant ''CUInt "CONFIG_DEPTHLIMIT"
constant ''CUInt "CONFIG_EFFECTIVE_LINKSIZE"  -- ^ @since 2.2.3
constant ''CUInt "CONFIG_HEAPLIMIT"
constant ''CUInt "CONFIG_JIT"
constant ''CUInt "CONFIG_JITTARGET"
constant ''CUInt "CONFIG_LINKSIZE"
constant ''CUInt "CONFIG_MATCHLIMIT"
constant ''CUInt "CONFIG_NEWLINE"
constant ''CUInt "CONFIG_NEVER_BACKSLASH_C"
constant ''CUInt "CONFIG_PARENSLIMIT"
constant ''CUInt "CONFIG_STACKRECURSE"
constant ''CUInt "CONFIG_TABLES_LENGTH"
constant ''CUInt "CONFIG_UNICODE_VERSION"
constant ''CUInt "CONFIG_UNICODE"
constant ''CUInt "CONFIG_VERSION"

-- * Compiling a pattern

foreign import capi safe "pcre2.h" pcre2_compile
    :: PCRE2_SPTR
    -> PCRE2_SIZE -- ^ Can be zero-terminated.  See below.
    -> CUInt
    -- ^ See "Main compile options" below for possible bit flags.
    -> Ptr CInt
    -> Ptr PCRE2_SIZE
    -> Ptr Pcre2_compile_context
    -> IO (Ptr Pcre2_code)

constant ''PCRE2_SIZE "ZERO_TERMINATED"

foreign import capi safe "pcre2.h" pcre2_code_free
    :: Ptr Pcre2_code
    -> IO ()

foreign import capi safe "pcre2.h &pcre2_code_free_8"
    pcre2_code_finalizer :: FinalizerPtr Pcre2_code

foreign import capi safe "pcre2.h" pcre2_code_copy
    :: Ptr Pcre2_code
    -> IO (Ptr Pcre2_code)

foreign import capi safe "pcre2.h" pcre2_code_copy_with_tables
    :: Ptr Pcre2_code
    -> IO (Ptr Pcre2_code)

-- ** Main compile options
constant ''CUInt "ANCHORED"
constant ''CUInt "ALLOW_EMPTY_CLASS"
constant ''CUInt "ALT_BSUX"
constant ''CUInt "ALT_CIRCUMFLEX"
constant ''CUInt "ALT_VERBNAMES"
constant ''CUInt "AUTO_CALLOUT"
constant ''CUInt "CASELESS"
constant ''CUInt "DOLLAR_ENDONLY"
constant ''CUInt "DOTALL"
constant ''CUInt "DUPNAMES"
constant ''CUInt "ENDANCHORED"
constant ''CUInt "EXTENDED"
constant ''CUInt "EXTENDED_MORE"
constant ''CUInt "FIRSTLINE"
constant ''CUInt "LITERAL"
constant ''CUInt "MATCH_INVALID_UTF"
constant ''CUInt "MATCH_UNSET_BACKREF"
constant ''CUInt "MULTILINE"
constant ''CUInt "NEVER_BACKSLASH_C"
constant ''CUInt "NEVER_UCP"
constant ''CUInt "NEVER_UTF"
constant ''CUInt "NO_AUTO_CAPTURE"
constant ''CUInt "NO_AUTO_POSSESS"
constant ''CUInt "NO_DOTSTAR_ANCHOR"
constant ''CUInt "NO_START_OPTIMIZE"
constant ''CUInt "NO_UTF_CHECK"
constant ''CUInt "UCP"
constant ''CUInt "UNGREEDY"
constant ''CUInt "USE_OFFSET_LIMIT"
constant ''CUInt "UTF"

-- ** Extra compile options
constant ''CUInt "EXTRA_ALLOW_SURROGATE_ESCAPES"
constant ''CUInt "EXTRA_ALT_BSUX"
constant ''CUInt "EXTRA_BAD_ESCAPE_IS_LITERAL"
constant ''CUInt "EXTRA_ESCAPED_CR_IS_LF"
constant ''CUInt "EXTRA_MATCH_LINE"
constant ''CUInt "EXTRA_MATCH_WORD"

-- * JIT compilation

foreign import capi safe "pcre2.h" pcre2_jit_compile
    :: Ptr Pcre2_code
    -> CUInt -- ^ See below for possible bit flags.
    -> IO CInt

constant ''CUInt "JIT_COMPLETE"
constant ''CUInt "JIT_PARTIAL_HARD"
constant ''CUInt "JIT_PARTIAL_SOFT"

-- | May be returned by 'pcre2_jit_compile'.
constant ''CInt "ERROR_JIT_BADOPTION"

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

constant ''CUInt "INFO_ALLOPTIONS"
constant ''CUInt "INFO_ARGOPTIONS"
constant ''CUInt "INFO_EXTRAOPTIONS"
constant ''CUInt "INFO_BACKREFMAX"
constant ''CUInt "INFO_BSR"
constant ''CUInt "INFO_CAPTURECOUNT"
constant ''CUInt "INFO_DEPTHLIMIT"
constant ''CUInt "INFO_FIRSTBITMAP"
constant ''CUInt "INFO_FIRSTCODETYPE"
constant ''CUInt "INFO_FIRSTCODEUNIT"
constant ''CUInt "INFO_FRAMESIZE"
constant ''CUInt "INFO_HASBACKSLASHC"
constant ''CUInt "INFO_HASCRORLF"
constant ''CUInt "INFO_HEAPLIMIT"
constant ''CUInt "INFO_JCHANGED"
constant ''CUInt "INFO_JITSIZE"
constant ''CUInt "INFO_LASTCODETYPE"
constant ''CUInt "INFO_LASTCODEUNIT"
constant ''CUInt "INFO_MATCHEMPTY"
constant ''CUInt "INFO_MATCHLIMIT"
constant ''CUInt "INFO_MAXLOOKBEHIND"
constant ''CUInt "INFO_MINLENGTH"
constant ''CUInt "INFO_NAMECOUNT"
constant ''CUInt "INFO_NAMEENTRYSIZE"
constant ''CUInt "INFO_NAMETABLE"
constant ''CUInt "INFO_NEWLINE"
constant ''CUInt "INFO_SIZE"

constant ''CInt "ERROR_UNSET"

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
getter "callout_block" [t| CUInt |] "version"
getter "callout_block" [t| CUInt |] "callout_number"
getter "callout_block" [t| CUInt |] "capture_top"
getter "callout_block" [t| CUInt |] "capture_last"
getter "callout_block" [t| CUInt |] "callout_flags"
-- ^ See below for possible bit flags.
constant ''CUInt "CALLOUT_STARTMATCH"
constant ''CUInt "CALLOUT_BACKTRACK"
getter "callout_block" [t| Ptr PCRE2_SIZE |] "offset_vector"
getter "callout_block" [t| PCRE2_SPTR |]     "mark"
getter "callout_block" [t| PCRE2_SPTR |]     "subject"
getter "callout_block" [t| PCRE2_SIZE |]     "subject_length"
getter "callout_block" [t| PCRE2_SIZE |]     "start_match"
getter "callout_block" [t| PCRE2_SIZE |]     "current_position"
getter "callout_block" [t| PCRE2_SIZE |]     "pattern_position"
getter "callout_block" [t| PCRE2_SIZE |]     "next_item_length"
getter "callout_block" [t| PCRE2_SIZE |]     "callout_string_offset"
getter "callout_block" [t| PCRE2_SIZE |]     "callout_string_length"
getter "callout_block" [t| PCRE2_SPTR |]     "callout_string"

-- *** @pcre2_callout_enumerate_block@
getter "callout_enumerate_block" [t| CUInt |]      "version"
getter "callout_enumerate_block" [t| PCRE2_SIZE |] "pattern_position"
getter "callout_enumerate_block" [t| PCRE2_SIZE |] "next_item_length"
getter "callout_enumerate_block" [t| CUInt |]      "callout_number"
getter "callout_enumerate_block" [t| PCRE2_SIZE |] "callout_string_offset"
getter "callout_enumerate_block" [t| PCRE2_SIZE |] "callout_string_length"
getter "callout_enumerate_block" [t| PCRE2_SPTR |] "callout_string"

-- *** @pcre2_substitute_callout_block@
getter "substitute_callout_block" [t| CUInt |]          "version"
getter "substitute_callout_block" [t| CUInt |]          "subscount"
getter "substitute_callout_block" [t| PCRE2_SPTR |]     "input"
getter "substitute_callout_block" [t| PCRE2_SPTR |]     "output"
getter "substitute_callout_block" [t| Ptr PCRE2_SIZE |] "ovector"
getter "substitute_callout_block" [t| CUInt |]          "oveccount"
getter "substitute_callout_block" [t| Ptr PCRE2_SIZE |] "output_offsets"
-- ^ Array of 2 (@PCRE2_SIZE[2]@)

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

foreign import capi safe "pcre2.h &pcre2_match_data_free_8"
    pcre2_match_data_finalizer :: FinalizerPtr Pcre2_match_data

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
constant ''CUInt "COPY_MATCHED_SUBJECT"
constant ''CUInt "NOTBOL"
constant ''CUInt "NOTEOL"
constant ''CUInt "NOTEMPTY"
constant ''CUInt "NOTEMPTY_ATSTART"
constant ''CUInt "NO_JIT"
constant ''CUInt "PARTIAL_HARD"
constant ''CUInt "PARTIAL_SOFT"

-- ** Match errors
constant ''CInt "ERROR_NOMATCH"
constant ''CInt "ERROR_PARTIAL"
constant ''CInt "ERROR_BADMAGIC"
constant ''CInt "ERROR_BADMODE"
constant ''CInt "ERROR_BADOFFSET"
constant ''CInt "ERROR_BADOPTION"
constant ''CInt "ERROR_BADUTFOFFSET"
constant ''CInt "ERROR_CALLOUT"
constant ''CInt "ERROR_DEPTHLIMIT"
constant ''CInt "ERROR_HEAPLIMIT"
constant ''CInt "ERROR_INTERNAL"
constant ''CInt "ERROR_JIT_STACKLIMIT"
constant ''CInt "ERROR_MATCHLIMIT"
constant ''CInt "ERROR_NOMEMORY"
constant ''CInt "ERROR_NULL"
-- *** UTF-16-specific errors
--
-- /Note: These were accidentally left in for version 2.2.0 which otherwise/
-- /changed `pcre2` from UTF-16 to UTF-8.  They are irrelevant and will be/
-- /removed in the next minor version./
--
-- | "Missing low surrogate at end of string"
constant ''CInt "ERROR_UTF16_ERR1"
-- | "Invalid low surrogate follows high surrogate"
constant ''CInt "ERROR_UTF16_ERR2"
-- | "Isolated low surrogate"
constant ''CInt "ERROR_UTF16_ERR3"

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
constant ''CUInt "DFA_SHORTEST"
constant ''CUInt "DFA_RESTART"

-- ** DFA matching errors
constant ''CInt "ERROR_DFA_UFUNC"
constant ''CInt "ERROR_DFA_UITEM"
constant ''CInt "ERROR_DFA_UCOND"
constant ''CInt "ERROR_DFA_UINVALID_UTF"
constant ''CInt "ERROR_DFA_WSSIZE"
constant ''CInt "ERROR_DFA_RECURSE"
constant ''CInt "ERROR_DFA_BADRESTART"

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

foreign import capi unsafe "pcre2.h" pcre2_substring_copy_bynumber
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

foreign import capi safe "pcre2.h" pcre2_substring_list_get
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

foreign import capi safe "pcre2.h" pcre2_substring_free
    :: Ptr PCRE2_UCHAR
    -> IO ()

constant ''CInt "ERROR_NOSUBSTRING"
constant ''CInt "ERROR_NOUNIQUESUBSTRING"
constant ''CInt "ERROR_UNAVAILABLE"

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
constant ''CUInt "SUBSTITUTE_REPLACEMENT_ONLY"
constant ''CUInt "SUBSTITUTE_GLOBAL"
constant ''CUInt "SUBSTITUTE_MATCHED"
constant ''CUInt "SUBSTITUTE_OVERFLOW_LENGTH"
constant ''CUInt "SUBSTITUTE_LITERAL"
constant ''CUInt "SUBSTITUTE_UNKNOWN_UNSET"
constant ''CUInt "SUBSTITUTE_UNSET_EMPTY"
constant ''CUInt "SUBSTITUTE_EXTENDED"

-- ** Substitution errors
constant ''CInt "ERROR_BADREPLACEMENT"
constant ''CInt "ERROR_BADREPESCAPE"
constant ''CInt "ERROR_BADSUBSTITUTION"
constant ''CInt "ERROR_BADSUBSPATTERN"

-- * Other information about a match

foreign import capi unsafe "pcre2.h" pcre2_get_mark
    :: Ptr Pcre2_match_data
    -> IO PCRE2_SPTR

foreign import capi unsafe "pcre2.h" pcre2_get_startchar
    :: Ptr Pcre2_match_data
    -> IO PCRE2_SIZE

-- * Obtaining a textual error message
foreign import capi unsafe "pcre2.h" pcre2_get_error_message
    :: CInt
    -> Ptr PCRE2_UCHAR
    -> PCRE2_SIZE
    -> IO CInt

-- * Iterating over all matches
foreign import capi safe "pcre2.h" pcre2_next_match
    :: Ptr Pcre2_match_data
    -> Ptr PCRE2_SIZE
    -> Ptr CUInt
    -> IO CInt

-- * Pattern serialization

foreign import capi safe "pcre2.h" pcre2_serialize_decode
    :: Ptr (Ptr Pcre2_code)      -- ^ codes
    -> CInt                      -- ^ number of codes
    -> Ptr CUInt                 -- ^ bytes
    -> Ptr Pcre2_general_context
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_serialize_encode
    :: Ptr (Ptr Pcre2_code)      -- ^ codes
    -> CInt                      -- ^ number of codes
    -> Ptr (Ptr CUInt)           -- ^ serialized bytes
    -> Ptr PCRE2_SIZE            -- ^ serialized size
    -> Ptr Pcre2_general_context
    -> IO CInt

foreign import capi safe "pcre2.h" pcre2_serialize_free
    :: Ptr CUChar
    -> IO ()

foreign import capi unsafe "pcre2.h" pcre2_serialize_get_number_of_codes
    :: Ptr CUChar
    -> IO CInt

constant ''CInt "ERROR_BADSERIALIZEDDATA"
constant ''CInt "ERROR_MIXEDTABLES"
