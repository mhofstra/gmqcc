/*
 * Copyright (C) 2012
 *     Dale Weiler
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
#include "gmqcc.h"

/*
 * inlined utility macros, O(1) lookup for size,links and next
 * block addresses.
 */
#define GMQCC_MEM_BLOCKSIZE(X,Y) ((X)->data[(Y)])
#define GMQCC_MEM_BLOCKNEXT(X,Y) ((X)->data[(Y) + (X)->data[(Y)]])
#define GMQCC_MEM_BLOCKLINK(X,Y) ((Y)           + (X)->data[(Y)])

mem_heap_t *mem_init(size_t size, const char *desc, const char *file, size_t line) {
    void       *data = malloc(sizeof(mem_heap_t) + size);
    mem_heap_t *heap = (mem_heap_t*)data;
    
    heap->data                 = (int*)(((unsigned char *)data) + sizeof(mem_heap_t));
    heap->size                 = size / sizeof(int);
    heap->left                 = 0;
    heap->data[0]              = heap->size - 1;
    heap->data[heap->size - 1] = heap->size;
    heap->file                 = file;
    heap->desc                 = desc;
    heap->line                 = line;
    heap->info                 = NULL;
    heap->allocations          = 0;
    heap->deallocations        = 0;
    heap->allocated            = 0;
    heap->deallocated          = 0;
    
    return heap;
}

void mem_destroy(mem_heap_t *heap, const char *file, size_t line) {
    void *base = NULL;
    
    util_debug("MEM", "Heap [%s] destroyed at: %s:%d\n",
        heap->desc, file, line);
        
    /* get the base pointer used for malloc */
    base = (void*)(((unsigned char *)heap->data) - sizeof(mem_heap_t));
    free(base);
}

void *mem_alloc(mem_heap_t *heap, size_t size, const char *file, size_t line) {
    size_t       find;
    size_t       self;
    size_t       prev;
    bool         stop  = true;
    mem_block_t *block = NULL;
    
    if (size == 0)
        return NULL;
        
    /* find correct size  */
    size += sizeof(mem_block_t);
    find  = size / sizeof(int);
    if     (size % sizeof(int) > 0) find++;
    
    /* allocate */
    prev = heap->left;
    self = heap->left;
    
    /* TODO: optimize */
    mem_alloc_test: {
        /* out of memory for heap? */
        if (heap->left == heap->size)
            return NULL;
        
        /* block size too small? */
        if ((size_t)GMQCC_MEM_BLOCKSIZE(heap, self) < find) {
            stop = false;
            prev = self;
            
            if ((size_t)GMQCC_MEM_BLOCKNEXT(heap, self) == heap->size)
                return NULL;
            else
                self = GMQCC_MEM_BLOCKNEXT(heap, self);
                
            /* retry */
            goto mem_alloc_test;
        }
        
        /* perfect fit :-) */
        if ((size_t)GMQCC_MEM_BLOCKSIZE(heap, self) == find) {
            if (stop)
                heap->left = GMQCC_MEM_BLOCKNEXT(heap, self);
            else
                heap->data[GMQCC_MEM_BLOCKLINK(heap, prev)] = GMQCC_MEM_BLOCKNEXT(heap, self);
        } else {
            /* not so perfect (size is larger) */
            if (stop) {
                heap->left =
                    (((size_t)GMQCC_MEM_BLOCKSIZE(heap, self) - find) == 1) ?
                        (size_t)GMQCC_MEM_BLOCKNEXT(heap, self) : self + find + 1;
            } else {
                heap->data [GMQCC_MEM_BLOCKLINK(heap, prev)] =
                    (((size_t)GMQCC_MEM_BLOCKSIZE(heap, self) - find) == 1) ?
                        (size_t)GMQCC_MEM_BLOCKNEXT(heap, self) : self + find + 1;
            }
            
            /* final */
            if (((size_t)GMQCC_MEM_BLOCKSIZE(heap, self) - find) == 1)
                heap->data[self] = find + 1;
            else
                heap->data[self + find + 1] = GMQCC_MEM_BLOCKSIZE(heap, self) - find - 1,
                heap->data[self]            = find;

        }
    }
    heap->allocations ++;
    heap->allocated   += size - sizeof(mem_block_t);
    
    /* 
     * store the "actual" size, line, and file
     * into the data for debug.
     */
    block       = (mem_block_t*) &heap->data[self + 1];
    block->size = size - sizeof(mem_block_t);
    block->line = line;
    block->file = file;
    block->prev = NULL;
    block->next = heap->info;
    
    if (heap->info)
        heap->info->prev = block;
    heap->info = block;
    
    util_debug("MEM", "allocation:   % 8u (bytes) address 0x%08X @ %s:%u\n",
        block->size,
        (void*)(block + 1),
        block->file,
        block->line
    );
    
    return (void*) (block + 1);
}

void mem_free(mem_heap_t *heap, void *ptr, const char *file, size_t line) {
    mem_block_t *block = ((mem_block_t*)ptr) - 1;
    int          free  = ((int *)block - (heap->data + 1));
    size_t       size  = block->size;
    size_t       self;
    size_t       prev;
    
    util_debug("MEM", "released:     % 8u (bytes) address 0x%08X @ %s:%u\n",
        block->size,
        ptr,
        file,
        line
    );
    
    /* rollback blocks */
    if (block->prev)
        block->prev->next = block->next;
    if (block->next)
        block->next->prev = block->prev;
    
    /* start at next ? */
    if (block == heap->info)
        heap->info = block->next;
    
    /* before first free block? */
    if ((size_t)free < heap->left) {
        /* immediately after block being freed? */
        if ((((size_t)GMQCC_MEM_BLOCKLINK(heap, free) + 1) == heap->left) && heap->left < heap->size)
            heap->data[free] += (heap->data[heap->left] + 1);
        else
            heap->data[GMQCC_MEM_BLOCKLINK(heap, free)] = heap->left;
        /* update availble amount */
        heap->left = free;
    } else {
        /* isn't before first free block? */
        prev = self = heap->left;
        
        /* TODO: optimize */
        while (self < (size_t)free) {
            prev = self;
            self = GMQCC_MEM_BLOCKNEXT(heap, self);
        }
        
        /* immediately before block being freed? */
        if ((GMQCC_MEM_BLOCKLINK(heap, prev) + 1) == (size_t)free)  {
            /* deframentation of free space */
            heap->data[prev] += (heap->data[free] + 1);
            /* immediately after block being freed? */
            if ((((size_t)GMQCC_MEM_BLOCKLINK(heap, free) + 1) == self) && self < heap->size) {
                /* defragmentation of free space */
                heap->data[prev] += (heap->data[self] + 1);
            } else {
                heap->data[GMQCC_MEM_BLOCKLINK(heap, free)] = self;
            }
        } else {
            heap->data[GMQCC_MEM_BLOCKLINK(heap, prev)] = free;
            heap->data[GMQCC_MEM_BLOCKLINK(heap, free)] = self;
        }
    }
    heap->deallocations ++;
    heap->deallocated   += size;
}

/*
 * TODO: optimize by inlining the alloc/free as duped code eliminating
 * unneeded logic.
 */
void *mem_realloc(mem_heap_t *heap, void *ptr, size_t size, const char *file, size_t line) {        
    void        *new = mem_alloc(heap, size, file, line);
    mem_block_t *old;
    
    if (!ptr)
        return new;
    if (!size) {
        mem_free(heap, ptr, file, line);
        return NULL;
    }
        
    old = ((mem_block_t*)ptr) - 1;
    
    util_debug("MEM", "reallocating: % 8u (bytes) address 0x%08X @ %s:%u to %d (bytes)\n",
        old->size,
        ptr,
        old->file,
        old->line,
        size
    );
    memcpy(new, ptr, old->size);
    mem_free(heap, ptr, file, line);
    return new;
}

/*
 * TODO: mutual exclusion for threadsafety if our compiler decides
 * to go multithreaded.
 * 
 * Note: we cannot use a vector for this vector requires mem_a
 * so .. to combat this we just use a fixed size.  Since we know
 * statically how many we need.
 */
static mem_heap_t *mem_heaps[1] = { NULL };
static size_t      mem_bumps    = 0;
mem_heap_t *mem_heap_add(const char *desc, size_t size, const char *file, size_t line) {
    mem_heap_t  *heap    = mem_init(size, desc, file, line);
    mem_heaps[mem_bumps] = heap;
    util_debug("MEM", "Added new heap: %s, %lu (bytes)\n",
        desc, size);
    mem_bumps++;
    return heap;
}

void mem_heap_dump(mem_heap_t *heap) {
    mem_block_t *block = heap->info;
    
    util_debug("MEM", "Heap Report:\n");
    util_debug("MEM", "    Description: %s\n", heap->desc);
    util_debug("MEM", "    Initialized: %s:%d\n", heap->file, heap->line);
    util_debug("MEM", "    Size:        %d (bytes)\n", heap->size * sizeof(int));
    
    while (block) {
        util_debug("MEM", "    Lost: % 8u (bytes) at %s:%u\n",
            block->size,
            block->file,
            block->line
        );
        
        block = block->next;
    }
    
    util_debug("MEM", "    Memory Report:\n");
    util_debug("MEM", "        Total allocations:   %llu\n", heap->allocations);
    util_debug("MEM", "        Total deallocations: %llu\n", heap->deallocations);
    util_debug("MEM", "        Total allocated:     %llu (bytes)\n", heap->allocated);
    util_debug("MEM", "        Total deallocated:   %llu (bytes)\n", heap->deallocated);
    util_debug("MEM", "\n");
    util_debug("MEM", "        Leaks found:\n");
    util_debug("MEM", "            Lost: %llu (bytes)\n", (heap->allocated   - heap->deallocated));
    util_debug("MEM", "            From: %d (alllocations)\n",  (heap->allocations - heap->deallocations));
}

/*
 * Cleverly dumps all information for all heaps with basic
 * iteration.  Since mem_heaps is only a vector (for now).
 */
void mem_dump() {
    size_t select = 0;
    for (; select < mem_bumps; select++) {
        mem_heap_dump(mem_heaps[select]);
    }
}

void mem_destroyall(const char *file, size_t line) {
    size_t select = 0;
    for (; select < mem_bumps; select++) {
        mem_destroy(mem_heaps[select], file, line);
    }
}

#undef GMQCC_MEM_BLOCKLINK
#undef GMQCC_MEM_BLOCKNEXT
#undef GMQCC_MEM_BLOCKSIZE
