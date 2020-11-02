#include <pcre2.h>

#ifndef GETTERS_H
#define GETTERS_H

#ifdef GETTER
#error GETTER already defined
#endif
#define GETTER(src, field, ret) \
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
GETTER(callout_block, version,               uint32_t)
GETTER(callout_block, callout_number,        uint32_t)
GETTER(callout_block, capture_top,           uint32_t)
GETTER(callout_block, capture_last,          uint32_t)
GETTER(callout_block, callout_flags,         uint32_t)
GETTER(callout_block, offset_vector,         PCRE2_SIZE *)
GETTER(callout_block, mark,                  PCRE2_SPTR)
GETTER(callout_block, subject,               PCRE2_SPTR)
GETTER(callout_block, subject_length,        PCRE2_SIZE)
GETTER(callout_block, start_match,           PCRE2_SIZE)
GETTER(callout_block, current_position,      PCRE2_SIZE)
GETTER(callout_block, pattern_position,      PCRE2_SIZE)
GETTER(callout_block, next_item_length,      PCRE2_SIZE)
GETTER(callout_block, callout_string_offset, PCRE2_SIZE)
GETTER(callout_block, callout_string_length, PCRE2_SIZE)
GETTER(callout_block, callout_string,        PCRE2_SPTR)

GETTER(callout_enumerate_block, version,               uint32_t)
GETTER(callout_enumerate_block, pattern_position,      PCRE2_SIZE)
GETTER(callout_enumerate_block, next_item_length,      PCRE2_SIZE)
GETTER(callout_enumerate_block, callout_number,        uint32_t)
GETTER(callout_enumerate_block, callout_string_offset, PCRE2_SIZE)
GETTER(callout_enumerate_block, callout_string_length, PCRE2_SIZE)
GETTER(callout_enumerate_block, callout_string,        PCRE2_SPTR)

GETTER(substitute_callout_block, version,        uint32_t)
GETTER(substitute_callout_block, subscount,      uint32_t)
GETTER(substitute_callout_block, input,          PCRE2_SPTR)
GETTER(substitute_callout_block, output,         PCRE2_SPTR)
GETTER(substitute_callout_block, ovector,        PCRE2_SIZE *)
GETTER(substitute_callout_block, oveccount,      uint32_t)
GETTER(substitute_callout_block, output_offsets, PCRE2_SIZE *) // array of 2

#undef GETTER

#endif