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
#define MEM_HEAPS      32
#define MEM_HEAPLINE   24             /* line MEM_HEAP is defined on above     */
#define MEM_ALLOC(X)   malloc (X)     /* internal allocation function to use   */
#define MEM_FREE(X)    free   (X)     /* internal deallocation function to use */

static mem_heap_t *mem_heap_data[MEM_HEAPS];
static size_t      mem_heap_size = 0;

mem_heap_t *_mem_new(const char *name, const char *file, const char *func, size_t line) {
    mem_heap_t *heap = NULL;

    /* allocate the data */
    if (!mem_heap_size) {
        size_t i = 0;
        for (; i < sizeof(mem_heap_data) / sizeof (*mem_heap_data); i++)
            mem_heap_data[i] = (mem_heap_t*) MEM_ALLOC (sizeof(mem_heap_t));
    }

    /* increase MAX_HEAP .. not enough stack space for another */
    if (mem_heap_size + 1 >= sizeof(mem_heap_data) / sizeof(*mem_heap_data)) {
        con_err("Too many heaps, increase MEM_HEAPS, defined at %s:%d\n", __FILE__, MEM_HEAPLINE);
        abort();
    }

    heap       = mem_heap_data[mem_heap_size ++];
    heap->name = name;
    heap->func = func;
    heap->file = file;
    heap->line = line;
    heap->info = NULL;

    return heap;
}

void *_mem_alloc(mem_heap_t *heap, size_t size, const char *file, const char *func, size_t line) {
    mem_info_t *info = (mem_info_t*) MEM_ALLOC (sizeof(mem_info_t) + size);
    void       *data = (void*)(info + 1);

    if (!info) return NULL;


    info->func = func;
    info->file = file;
    info->line = line;
    info->size = size;
    info->prev = NULL;
    info->next = heap->info;
    info->heap = (void*)heap;

    if (heap->info)
        heap->info->prev = info;

    heap->info        = info;
    heap->allocated  += size;
    heap->allocations++;

    return data;
}

/* don't need the heap as argument */
void _mem_free(mem_heap_t *heap, void *data, const char *file, const char *func, size_t line) {
    mem_info_t *info = NULL;
    if (!data) return;

    info = ((mem_info_t*)data - 1);

    /* handle mismatch, but warn for it */
    if (heap != (mem_heap_t*)info->heap) {
        util_debug("MEM", "Warning: mismatched free. Allocated from heap: %s, attempted free from heap: %s [%s:%s:%d]\n",
            heap->name, ((mem_heap_t*)info->heap)->name,
            file, func, line
        );

        /* redirect */
        heap = (mem_heap_t*)info->heap;
    }

    heap->deallocated   += info->size;
    heap->deallocations ++;

    if (info->prev) info->prev->next = info->next;
    if (info->next) info->next->prev = info->prev;

    if (info == heap->info)
        heap->info = info->next;

    MEM_FREE (info);
}

/*
 * Easily obtain memory size for things like _vec (no need to duplicate
 * data for this).
 */ 
size_t mem_size(void *data) {
    return ((mem_info_t*)data - 1)->size;
}

void *_mem_realloc(mem_heap_t *heap, void *data, size_t size, const char *file, const char *func, size_t line) {
    mem_info_t *oldinfo = NULL;
    mem_info_t *newinfo = NULL;

    if (!data)
        return _mem_alloc(heap, size, file, func, line);
    if (!size) {
        _mem_free(heap, data, file, func, line);
        return NULL;
    }

    oldinfo = ((mem_info_t*)data - 1);
    newinfo = ((mem_info_t*) MEM_ALLOC (sizeof(mem_info_t) + size));

    if (!newinfo) {
        _mem_free(heap, oldinfo + 1, file, func, line);
        return NULL;
    }

    memcpy(newinfo+1, oldinfo+1, oldinfo->size);

    if (oldinfo->prev) oldinfo->prev->next = oldinfo->next;
    if (oldinfo->next) oldinfo->next->prev = oldinfo->prev;

    if (oldinfo == heap->info)
        heap->info = oldinfo->next;

    newinfo->func = func;
    newinfo->file = file;
    newinfo->line = line;
    newinfo->size = size;
    newinfo->prev = NULL;
    newinfo->next = heap->info;
    newinfo->heap = (void*)heap;

    if (heap->info)
        heap->info->prev = newinfo;
    heap->info = newinfo;

    heap->allocated -= oldinfo->size;
    heap->allocated += newinfo->size;

    MEM_FREE (oldinfo);

    return newinfo + 1;
}

void _mem_freeall(mem_heap_t *heap, const char *file, const char *func, size_t line) {
    mem_info_t *info;
    for (info = heap->info; info; info = info->next)
        _mem_free (heap, info + 1, file, func, line);
}

void mem_destroy() {
    size_t i = 0;
    for (; i < mem_heap_size; i++)
        MEM_FREE (mem_heap_data[i]);
}

void mem_dump() {
    mem_info_t *info          = NULL;
    mem_heap_t *heap          = NULL;
    const char *bestlargename = NULL;
    const char *bestsmallname = NULL;
    uint64_t    bestlarge     = 0;
    uint64_t    bestsmall     = 0;
    size_t      next          = 0;

    /* Find largest heap */
    for (next = 0, heap = *mem_heap_data; next < mem_heap_size; heap = mem_heap_data[next]) {
        if (heap->allocated > bestlarge) {
            bestlarge     = heap->allocated;
            bestsmall     = bestlarge;
            bestlargename = heap->name;
            bestsmallname = bestlargename;
        }
        next++;
    }

    /* Find smallest heap */
    for (next = 0, heap = *mem_heap_data; next < mem_heap_size; heap = mem_heap_data[next]) {
        if (heap->allocated < bestsmall) {
            bestsmall     = heap->allocated;
            bestsmallname = heap->name;
        }
        next++;
    }

            
    util_debug("MEM", "Memory Report:\n");
    util_debug("MEM", "    Heaps:    %u\n", mem_heap_size);
    util_debug("MEM", "    Largest:  %s: %llu (bytes)\n", bestlargename, bestlarge);
    util_debug("MEM", "    Smallest: %s: %llu (bytes)\n", bestsmallname, bestsmall);
    util_debug("MEM", "\n");
    for (next = 0, heap = *mem_heap_data; next < mem_heap_size; heap = mem_heap_data[next]) {
        /* Heap Report: */
        util_debug("MEM", "    Heap Report:\n");
        util_debug("MEM", "        Name:                %s\n"           , heap->name);
        util_debug("MEM", "        Initializated At:    %s:%u in %s\n"  , heap->file, heap->line, heap->func);
        util_debug("MEM", "        Total Allocations:   %llu\n"         , heap->allocations);
        util_debug("MEM", "        Total Deallocations: %llu\n"         , heap->deallocations);
        util_debug("MEM", "        Total Allocated:     %llu (bytes)\n" , heap->allocated);
        util_debug("MEM", "        Total Deallocated:   %llu (bytes)\n" , heap->deallocated);
        util_debug("MEM", "        Leak Report:\n");
        util_debug("MEM", "            Lost: %llu (bytes)\n"      , heap->allocated   - heap->deallocated);
        util_debug("MEM", "            From: %llu (allocations)\n", heap->allocations - heap->deallocations);

        /* there was leaks */
        if (heap->allocated - heap->deallocated > 0) {
            util_debug("MEM", "            Leak Traceback:\n");
            for (info = heap->info; info; info = info->next) {
                util_debug("MEM", "                Lost: % 8u (bytes) at %s:%u in %s\n",
                    info->size,
                    info->file,
                    info->line,
                    info->func
                );
            }
        }
        next++;
    }
}
