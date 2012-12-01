/*
 * Copyright (C) 2012
 *     Dale Weiler
 *     Wolfgang Bumiller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <stdarg.h>
#include <errno.h>
#include "gmqcc.h"
mem_heap_t *util_heap;

/*
 * Some string utility functions, because strdup uses malloc, and we want
 * to track all memory (without replacing malloc).
 */
char *util_strdup(mem_heap_t *heap, const char *s) {
    size_t  len = 0;
    char   *ptr = NULL;

    if (!s)
        return NULL;

    if ((len = strlen(s)) && (ptr = mem_alloc(heap, len+1))) {
        memcpy(ptr, s, len);
        ptr[len] = '\0';
    }
    return ptr;
}

/*
 * Remove quotes from a string, escapes from \ in string
 * as well.  This function shouldn't be used to create a
 * char array that is later freed (it uses pointer arith)
 */
char *util_strrq(const char *s) {
    char *dst = (char*)s;
    char *src = (char*)s;
    char  chr;
    while ((chr = *src++) != '\0') {
        if (chr == '\\') {
            *dst++ = chr;
            if ((chr = *src++) == '\0')
                break;
            *dst++ = chr;
        } else if (chr != '"')
            *dst++ = chr;
    }
    *dst = '\0';
    return dst;
}

/*
 * Returns true if string is all uppercase, otherwise
 * it returns false.
 */
bool util_strupper(const char *str) {
    while (*str) {
        if(!isupper(*str))
            return false;
        str++;
    }
    return true;
}

/*
 * Returns true if string is all digits, otherwise
 * it returns false.
 */
bool util_strdigit(const char *str) {
    while (*str) {
        if(!isdigit(*str))
            return false;
        str++;
    }
    return true;
}

bool util_strncmpexact(const char *src, const char *ned, size_t len) {
    return (!strncmp(src, ned, len) && !src[len]);
}

void util_debug(const char *area, const char *ms, ...) {
    va_list  va;
    if (!opts_debug)
        return;

    if (!strcmp(area, "MEM") && !opts_memchk)
        return;

    va_start(va, ms);
    con_out ("[%s] ", area);
    con_vout(ms, va);
    va_end  (va);
}

/*
 * Endianess swapping, all data must be stored little-endian.  This
 * reorders by stride and length, much nicer than other functions for
 * certian-sized types like short or int.
 */
void util_endianswap(void *m, int s, int l) {
    size_t w = 0;
    size_t i = 0;

    /* ignore if we're already LE */
    if(*((char *)&s))
        return;

    for(; w < (size_t)l; w++) {
        for(;  i < (size_t)(s << 1); i++) {
            unsigned char *p = (unsigned char *)m+w*s;
            unsigned char  t = p[i];
            p[i]             = p[s-i-1];
            p[s-i-1]         = t;
        }
    }
}

/*
 * CRC algorithms vary in the width of the polynomial, the value of said polynomial,
 * the initial value used for the register, weather the bits of each byte are reflected
 * before being processed, weather the algorithm itself feeds input bytes through the
 * register or XORs them with a byte from one end and then straight into the table, as
 * well as (but not limited to the idea of reflected versions) where the final register
 * value becomes reversed, and finally weather the value itself is used to XOR the final
 * register value.  AS such you can already imagine how painfully annoying CRCs are,
 * of course we stand to target Quake, which expects it's certian set of rules for proper
 * calculation of a CRC.
 *
 * In most traditional CRC algorithms on uses a reflected table driven method where a value
 * or register is reflected if it's bits are swapped around it's center.  For example:
 * take the bits 0101 is the 4-bit reflection of 1010, and respectfully 0011 would be the
 * reflection of 1100. Quakle however expects a NON-Reflected CRC on the output, but still
 * requires a final XOR on the values (0xFFFF and 0x0000) this is a standard CCITT CRC-16
 * which I respectfully as a programmer don't agree with.
 *
 * So now you know what we target, and why we target it, despite how unsettling it may seem
 * but those are what Quake seems to request.
 */

static const uint16_t util_crc16_table[] = {
    0x0000,0x1021,0x2042,0x3063,0x4084,0x50A5,0x60C6,0x70E7,0x8108,0x9129,0xA14A,0xB16B,
    0xC18C,0xD1AD,0xE1CE,0xF1EF,0x1231,0x0210,0x3273,0x2252,0x52B5,0x4294,0x72F7,0x62D6,
    0x9339,0x8318,0xB37B,0xA35A,0xD3BD,0xC39C,0xF3FF,0xE3DE,0x2462,0x3443,0x0420,0x1401,
    0x64E6,0x74C7,0x44A4,0x5485,0xA56A,0xB54B,0x8528,0x9509,0xE5EE,0xF5CF,0xC5AC,0xD58D,
    0x3653,0x2672,0x1611,0x0630,0x76D7,0x66F6,0x5695,0x46B4,0xB75B,0xA77A,0x9719,0x8738,
    0xF7DF,0xE7FE,0xD79D,0xC7BC,0x48C4,0x58E5,0x6886,0x78A7,0x0840,0x1861,0x2802,0x3823,
    0xC9CC,0xD9ED,0xE98E,0xF9AF,0x8948,0x9969,0xA90A,0xB92B,0x5AF5,0x4AD4,0x7AB7,0x6A96,
    0x1A71,0x0A50,0x3A33,0x2A12,0xDBFD,0xCBDC,0xFBBF,0xEB9E,0x9B79,0x8B58,0xBB3B,0xAB1A,
    0x6CA6,0x7C87,0x4CE4,0x5CC5,0x2C22,0x3C03,0x0C60,0x1C41,0xEDAE,0xFD8F,0xCDEC,0xDDCD,
    0xAD2A,0xBD0B,0x8D68,0x9D49,0x7E97,0x6EB6,0x5ED5,0x4EF4,0x3E13,0x2E32,0x1E51,0x0E70,
    0xFF9F,0xEFBE,0xDFDD,0xCFFC,0xBF1B,0xAF3A,0x9F59,0x8F78,0x9188,0x81A9,0xB1CA,0xA1EB,
    0xD10C,0xC12D,0xF14E,0xE16F,0x1080,0x00A1,0x30C2,0x20E3,0x5004,0x4025,0x7046,0x6067,
    0x83B9,0x9398,0xA3FB,0xB3DA,0xC33D,0xD31C,0xE37F,0xF35E,0x02B1,0x1290,0x22F3,0x32D2,
    0x4235,0x5214,0x6277,0x7256,0xB5EA,0xA5CB,0x95A8,0x8589,0xF56E,0xE54F,0xD52C,0xC50D,
    0x34E2,0x24C3,0x14A0,0x0481,0x7466,0x6447,0x5424,0x4405,0xA7DB,0xB7FA,0x8799,0x97B8,
    0xE75F,0xF77E,0xC71D,0xD73C,0x26D3,0x36F2,0x0691,0x16B0,0x6657,0x7676,0x4615,0x5634,
    0xD94C,0xC96D,0xF90E,0xE92F,0x99C8,0x89E9,0xB98A,0xA9AB,0x5844,0x4865,0x7806,0x6827,
    0x18C0,0x08E1,0x3882,0x28A3,0xCB7D,0xDB5C,0xEB3F,0xFB1E,0x8BF9,0x9BD8,0xABBB,0xBB9A,
    0x4A75,0x5A54,0x6A37,0x7A16,0x0AF1,0x1AD0,0x2AB3,0x3A92,0xFD2E,0xED0F,0xDD6C,0xCD4D,
    0xBDAA,0xAD8B,0x9DE8,0x8DC9,0x7C26,0x6C07,0x5C64,0x4C45,0x3CA2,0x2C83,0x1CE0,0x0CC1,
    0xEF1F,0xFF3E,0xCF5D,0xDF7C,0xAF9B,0xBFBA,0x8FD9,0x9FF8,0x6E17,0x7E36,0x4E55,0x5E74,
    0x2E93,0x3EB2,0x0ED1,0x1EF0
};

uint16_t util_crc16(uint16_t current, const char *k, size_t len) {
    register uint16_t h= current;
    for (; len; --len, ++k)
        h = util_crc16_table[(h>>8)^((unsigned char)*k)]^(h<<8);
    return h;
}

/*
 * Implements libc getline for systems that don't have it, which is
 * assmed all.  This works the same as getline().
 */
int util_getline(char **lineptr, size_t *n, FILE *stream) {
    int   chr;
    int   ret;
    char *pos;

    if (!lineptr || !n || !stream)
        return -1;
    if (!*lineptr) {
        if (!(*lineptr = (char*)mem_alloc(util_heap, (*n=64))))
            return -1;
    }

    chr = *n;
    pos = *lineptr;

    for (;;) {
        int c = getc(stream);

        if (chr < 2) {
            *n += (*n > 16) ? *n : 64;
            chr = *n + *lineptr - pos;
            if (!(*lineptr = (char*)mem_realloc(util_heap, *lineptr, *n)))
                return -1;
            pos = *n - chr + *lineptr;
        }

        if (ferror(stream))
            return -1;
        if (c == EOF) {
            if (pos == *lineptr)
                return -1;
            else
                break;
        }

        *pos++ = c;
        chr--;
        if (c == '\n')
            break;
    }
    *pos = '\0';
    return (ret = pos - *lineptr);
}

size_t util_strtocmd(const char *in, char *out, size_t outsz) {
    size_t sz = 1;
    for (; *in && sz < outsz; ++in, ++out, ++sz)
        *out = (*in == '-') ? '_' : (isalpha(*in) && !isupper(*in)) ? *in + 'A' - 'a': *in;
    *out = 0;
    return sz-1;
}

size_t util_strtononcmd(const char *in, char *out, size_t outsz) {
    size_t sz = 1;
    for (; *in && sz < outsz; ++in, ++out, ++sz)
        *out = (*in == '_') ? '-' : (isalpha(*in) && isupper(*in)) ? *in + 'a' - 'A' : *in;
    *out = 0;
    return sz-1;
}


FILE *util_fopen(const char *filename, const char *mode)
{
#ifdef WIN32
    FILE *out;
    if (fopen_s(&out, filename, mode) != 0)
        return NULL;
    return out;
#else
    return fopen(filename, mode);
#endif
}

void _util_vec_grow(mem_heap_t *heap, void **a, size_t i, size_t s) {
    size_t m = *a ? 2*_vec_beg(*a)+i : i+1;
    void  *p = mem_realloc(heap, (*a ? _vec_raw(*a) : NULL), s * m + sizeof(size_t)*2);
    if (!*a)
        ((size_t*)p)[1] = 0;
    *a = (void*)((size_t*)p + 2);
    _vec_beg(*a) = m;
}


/*
 * Hash table for generic data, based on dynamic memory allocations
 * all around.  This is the internal interface, please look for
 * EXPOSED INTERFACE comment below
 */
typedef struct hash_node_t {
    char               *key;   /* the key for this node in table */
    void               *value; /* pointer to the data as void*   */
    struct hash_node_t *next;  /* next node (linked list)        */
} hash_node_t;

/*
 * x86 and x86_64 optimized murmur hash functions for the hashtable
 * we have individual implementations for optimal performance.
 *
 * Forced inlined as we wrap these up in the actual utility function
 * below.  These should be autovectorized by gcc.
 */
#ifdef __x86_64__
GMQCC_INLINE uint32_t util_hthashfunc(hash_table_t *ht, const char *key, register size_t seed) {
    const uint64_t       mix   = 0xC6A4A7935BD1E995;
    const int            rot   = 47;
    size_t               size  = strlen(key);
    uint64_t             hash  = seed ^ (size - mix);
    uint64_t             alias = 0;
    const uint64_t      *beg   = (const uint64_t*)key;
    const uint64_t      *end   = beg + (size / 8);
    const unsigned char *final = NULL;

    while (beg != end) {
        alias = *beg++;

        alias *= mix;
        alias ^= alias >> rot;
        alias *= mix;

        hash  ^= alias;
        hash  *= mix;
    }

    final = (const unsigned char *)beg;

    switch (size & 7) {
        case 7: hash ^= (uint64_t)(final[6]) << 48;
        case 6: hash ^= (uint64_t)(final[5]) << 40;
        case 5: hash ^= (uint64_t)(final[4]) << 32;
        case 4: hash ^= (uint64_t)(final[3]) << 24;
        case 3: hash ^= (uint64_t)(final[2]) << 16;
        case 2: hash ^= (uint64_t)(final[1]) << 8;
        case 1: hash ^= (uint64_t)(final[0]);
                hash *= mix;
    }

    hash ^= hash >> rot;
    hash *= mix;
    hash ^= hash >> rot;

    return (uint32_t)(hash % ht->size);
}

#else
GMQCC_INLINE uint32_t util_hthashfunc(hash_table_t *ht, const char *key, register size_t seed) {
    const uint32_t       mix   = 0x5BD1E995;
    const uint32_t       rot   = 24;
    size_t               size  = strlen(key);
    uint32_t             hash  = seed ^ size;
    uint32_t             alias = 0;
    const unsigned char *data  = (const unsigned char*)ket;

    while (size >= 4) {
        alias = *(uint32_t*)data;

        alias *= mix;
        alias ^= alias >> rot;
        alias *= mix;

        hash  *= mix;
        hash  ^= alias;

        data += 4;
        size -= 4;
    }

    switch (size) {
        case 3: hash ^= data[2] << 16;
        case 2: hash ^= data[1] << 8;
        case 1: hash ^= data[0];
                hash *= mix;
    }

    hash ^= hash >> 13;
    hash *= mix;
    hash ^= hash >> 15;

    return hash % ht->size;
}
#endif

/* we use the crc table as seeds for the murmur hash :P */
size_t util_hthash(hash_table_t *ht, const char *key) {
    register size_t hash = util_hthashfunc(ht, key, util_crc16_table[1]);
    return hash;
}

hash_node_t *_util_htnewpair(mem_heap_t *heap, const char *key, void *value) {
    hash_node_t *node;
    if (!(node = mem_alloc(heap, sizeof(hash_node_t))))
        return NULL;

    if (!(node->key = util_strdup(heap, key))) {
        mem_free(heap, node);
        return NULL;
    }

    node->value = value;
    node->next  = NULL;

    return node;
}

/*
 * EXPOSED INTERFACE for the hashtable implementation
 * util_htnew(size)                             -- to make a new hashtable
 * util_htset(table, key, value, sizeof(value)) -- to set something in the table
 * util_htget(table, key)                       -- to get something from the table
 * util_htdel(table)                            -- to delete the table
 */
hash_table_t *util_htnew(size_t size, mem_heap_t *heap) {
    hash_table_t *hashtable = NULL;
    if (size < 1)
        return NULL;

    if (!(hashtable = mem_alloc(heap, sizeof(hash_table_t))))
        return NULL;

    if (!(hashtable->table = mem_alloc(heap, sizeof(hash_node_t*) * size))) {
        mem_free(heap, hashtable);
        return NULL;
    }

    hashtable->size = size;
    hashtable->heap = heap;
    memset(hashtable->table, 0, sizeof(hash_node_t*) * size);

    return hashtable;
}

void util_htseth(hash_table_t *ht, const char *key, size_t bin, void *value) {
    hash_node_t *newnode = NULL;
    hash_node_t *next    = NULL;
    hash_node_t *last    = NULL;

    next = ht->table[bin];

    while (next && next->key && strcmp(key, next->key) > 0)
        last = next, next = next->next;

    /* already in table, do a replace */
    if (next && next->key && strcmp(key, next->key) == 0) {
        next->value = value;
    } else {
        /* not found, grow a pair man :P */
        newnode = _util_htnewpair(ht->heap, key, value);
        if (next == ht->table[bin]) {
            newnode->next  = next;
            ht->table[bin] = newnode;
        } else if (!next) {
            last->next = newnode;
        } else {
            newnode->next = next;
            last->next = newnode;
        }
    }
}

void util_htset(hash_table_t *ht, const char *key, void *value) {
    util_htseth(ht, key, util_hthash(ht, key), value);
}

void *util_htgeth(hash_table_t *ht, const char *key, size_t bin) {
    hash_node_t *pair = ht->table[bin];

    while (pair && pair->key && strcmp(key, pair->key) > 0)
        pair = pair->next;

    if (!pair || !pair->key || strcmp(key, pair->key) != 0)
        return NULL;

    return pair->value;
}

void *util_htget(hash_table_t *ht, const char *key) {
    return util_htgeth(ht, key, util_hthash(ht, key));
}

/*
 * Free all allocated data in a hashtable, this is quite the amount
 * of work.  We need to unroll this table a bit since a lot of time
 * is wasted here.
 */
void util_htdel(hash_table_t *ht) {
    size_t i  = 0;
    for (; i < ht->size; i++) {
        hash_node_t *n = ht->table[i];
        hash_node_t *p;

        /* free in list */
        while (n) {
            if (n->key)
                mem_free(ht->heap, n->key);
            p = n;
            n = n->next;
            mem_free(ht->heap, p);
        }
    }
    /* free table */
    mem_free(ht->heap, ht->table);
    mem_free(ht->heap, ht);
}
