// Functions to get PCRE2 struct fields.

#ifndef GETTERS_H
#define GETTERS_H

#include <pcre2.h>

#ifdef GETTER
#error GETTER already defined
#endif
#define GETTER(src, ret, field) \
    ret pcre2_##src##_##field(pcre2_##src *block) \
    { \
        return block->field; \
    }

/*
    For example, the first of these produces the following code:

    uint32_t pcre2_callout_block_version(pcre2_callout_block *block)
    {
        return block->version;
    }
*/
GETTER(callout_block, uint32_t,     version)
GETTER(callout_block, uint32_t,     callout_number)
GETTER(callout_block, uint32_t,     capture_top)
GETTER(callout_block, uint32_t,     capture_last)
GETTER(callout_block, uint32_t,     callout_flags)
GETTER(callout_block, PCRE2_SIZE *, offset_vector)
GETTER(callout_block, PCRE2_SPTR,   mark)
GETTER(callout_block, PCRE2_SPTR,   subject)
GETTER(callout_block, PCRE2_SIZE,   subject_length)
GETTER(callout_block, PCRE2_SIZE,   start_match)
GETTER(callout_block, PCRE2_SIZE,   current_position)
GETTER(callout_block, PCRE2_SIZE,   pattern_position)
GETTER(callout_block, PCRE2_SIZE,   next_item_length)
GETTER(callout_block, PCRE2_SIZE,   callout_string_offset)
GETTER(callout_block, PCRE2_SIZE,   callout_string_length)
GETTER(callout_block, PCRE2_SPTR,   callout_string)

GETTER(callout_enumerate_block, uint32_t,   version)
GETTER(callout_enumerate_block, PCRE2_SIZE, pattern_position)
GETTER(callout_enumerate_block, PCRE2_SIZE, next_item_length)
GETTER(callout_enumerate_block, uint32_t,   callout_number)
GETTER(callout_enumerate_block, PCRE2_SIZE, callout_string_offset)
GETTER(callout_enumerate_block, PCRE2_SIZE, callout_string_length)
GETTER(callout_enumerate_block, PCRE2_SPTR, callout_string)

GETTER(substitute_callout_block, uint32_t,     version)
GETTER(substitute_callout_block, uint32_t,     subscount)
GETTER(substitute_callout_block, PCRE2_SPTR,   input)
GETTER(substitute_callout_block, PCRE2_SPTR,   output)
GETTER(substitute_callout_block, PCRE2_SIZE *, ovector)
GETTER(substitute_callout_block, uint32_t,     oveccount)
GETTER(substitute_callout_block, PCRE2_SIZE *, output_offsets) // array of 2

#undef GETTER

#endif
