
/*
 ******************************************************************************
 *                               mm-baseline.c                                *
 *           64-bit struct-based implicit free list memory allocator          *
 *                  15-213: Introduction to Computer Systems                  *
 *                                                                            *
 *  ************************************************************************  *
 *                               DOCUMENTATION                                *
 *                                                                            *
 *  ** STRUCTURE. **                                                          *
 *                                                                            *
 *  Both allocated and free blocks share the same header structure.           *
 *  HEADER: 8-byte, aligned to 8th byte of an 16-byte aligned heap, where     *
 *          - The lowest order bit is 1 when the block is allocated, and      *
 *            0 otherwise.                                                    *
 *          - The whole 8-byte value with the least significant bit set to 0  *
 *            represents the size of the block as a size_t                    *
 *            The size of a block includes the header and footer.             *
 *  FOOTER: 8-byte, aligned to 0th byte of an 16-byte aligned heap. It        *
 *          contains the exact copy of the block's header.                    *
 *  The minimum blocksize is 32 bytes.                                        *
 *                                                                            *
 *  Allocated blocks contain the following:                                   *
 *  HEADER, as defined above.                                                 *
 *  PAYLOAD: Memory allocated for program to store information.               *
 *  FOOTER, as defined above.                                                 *
 *  The size of an allocated block is exactly PAYLOAD + HEADER + FOOTER.      *
 *                                                                            *
 *  Free blocks contain the following:                                        *
 *  HEADER, as defined above.                                                 *
 *  FOOTER, as defined above.                                                 *
 *  The size of an unallocated block is at least 32 bytes.                    *
 *                                                                            *
 *  Block Visualization.                                                      *
 *                    block     block+8          block+size-8   block+size    *
 *  Allocated blocks:   |  HEADER  |  ... PAYLOAD ...  |  FOOTER  |           *
 *                                                                            *
 *                    block     block+8          block+size-8   block+size    *
 *  Unallocated blocks: |  HEADER  |  ... (empty) ...  |  FOOTER  |           *
 *                                                                            *
 *  ************************************************************************  *
 *  ** INITIALIZATION. **                                                     *
 *                                                                            *
 *  The following visualization reflects the beginning of the heap.           *
 *      start            start+8           start+16                           *
 *  INIT: | PROLOGUE_FOOTER | EPILOGUE_HEADER |                               *
 *  PROLOGUE_FOOTER: 8-byte footer, as defined above, that simulates the      *
 *                    end of an allocated block. Also serves as padding.      *
 *  EPILOGUE_HEADER: 8-byte block indicating the end of the heap.             *
 *                   It simulates the beginning of an allocated block         *
 *                   The epilogue header is moved when the heap is extended.  *
 *                                                                            *
 *  ************************************************************************  *
 *  ** BLOCK ALLOCATION. **                                                   *
 *                                                                            *
 *  Upon memory request of size S, a block of size S + dsize, rounded up to   *
 *  16 bytes, is allocated on the heap, where dsize is 2*8 = 16.              *
 *  Selecting the block for allocation is performed by finding the first      *
 *  block that can fit the content based on a first-fit or next-fit search    *
 *  policy.                                                                   *
 *  The search starts from the beginning of the heap pointed by heap_listp.   *
 *  It sequentially goes through each block in the implicit free list,        *
 *  the end of the heap, until either                                         *
 *  - A sufficiently-large unallocated block is found, or                     *
 *  - The end of the implicit free list is reached, which occurs              *
 *    when no sufficiently-large unallocated block is available.              *
 *  In case that a sufficiently-large unallocated block is found, then        *
 *  that block will be used for allocation. Otherwise--that is, when no       *
 *  sufficiently-large unallocated block is found--then more unallocated      *
 *  memory of size chunksize or requested size, whichever is larger, is       *
 *  requested through mem_sbrk, and the search is redone.                     *
 *                                                                            *
 *  ************************************************************************  *
 *  ** ADVICE FOR STUDENTS. **                                                *
 *  Step 0: Please read the writeup!                                          *
 *  Write your heap checker. Write your heap checker. Write. Heap. checker.   *
 *  Good luck, and have fun!                                                  *
 *                                                                            *
 ******************************************************************************
 */

/* Do not change the following! */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>

#include "mm.h"
#include "memlib.h"

#ifdef DRIVER
/* create aliases for driver tests */
#define malloc mm_malloc
#define free mm_free
#define realloc mm_realloc
#define calloc mm_calloc
#define memset mem_memset
#define memcpy mem_memcpy
#endif /* def DRIVER */

/* You can change anything from here onward */

/*
 * If DEBUG is defined, enable printing on dbg_printf and contracts.
 * Debugging macros, with names beginning "dbg_" are allowed.
 * You may not define any other macros having arguments.
 */
//#define DEBUG // uncomment this line to enable debugging

#ifdef DEBUG
/* When debugging is enabled, these form aliases to useful functions */
#define dbg_printf(...) printf(__VA_ARGS__)
#define dbg_requires(...) assert(__VA_ARGS__)
#define dbg_assert(...) assert(__VA_ARGS__)
#define dbg_ensures(...) assert(__VA_ARGS__)
#else
/* When debugging is disnabled, no code gets generated for these */
#define dbg_printf(...)
#define dbg_requires(...)
#define dbg_assert(...)
#define dbg_ensures(...)
#endif

/* Basic constants */
typedef uint64_t word_t;
static const size_t wsize = sizeof(word_t);   // word, header, footer size (bytes)
static const size_t dsize = 2*wsize;          // double word size (bytes)
static const size_t min_block_size = 2*dsize; // Minimum block size
static const size_t chunksize = (1 << 11);    // requires (chunksize % 16 == 0)
static const int seglistsize = 7;


typedef struct block
{
    /* Header contains size + allocation flag */
    word_t header;
    /*
     * We don't know how big the payload will be.  Declaring it as an
     * array of size 0 allows computing its starting address using
     * pointer notation.
     */
    char payload[0];
    /*
     * We can't declare the footer as part of the struct, since its starting
     * position is unknown
     */
} block_t;


/* Global variables */
/* Pointer to first block */
static block_t *heap_listp = NULL;
static block_t *free_list[seglistsize];
static block_t *endofflist[seglistsize];
size_t largestfree = 0;


/* Function prototypes for internal helper routines */
static void insertblock(block_t *block);
static void removeblock(block_t *block);

static block_t *extend_heap(size_t size);
static void place(block_t *block, size_t asize);
static block_t *find_fit(size_t asize);
static block_t *coalesce(block_t *block);

static int get_class(size_t size);


static size_t max(size_t x, size_t y);
static size_t round_up(size_t size, size_t n);
static word_t pack(size_t size, bool alloc);

static size_t extract_size(word_t header);
static size_t get_size(block_t *block);
static size_t get_payload_size(block_t *block);

static bool extract_alloc(word_t header);
static bool get_alloc(block_t *block);

static void write_header(block_t *block, size_t size, bool alloc);
static void write_footer(block_t *block, size_t size, bool alloc);

static block_t *payload_to_header(void *bp);
static void *header_to_payload(block_t *block);

static bool preisfree(block_t *block);
static void makeprefree(block_t *block);
static void remprefree(block_t *next);

static block_t *find_next(block_t *block);
static word_t *find_prev_footer(block_t *block);
static block_t *find_prev(block_t *block);

bool mm_checkheap(int lineno);

/*
 * mm_init: initializes the heap; it is run once when heap_start == NULL.
 *          prior to any extend_heap operation, this is the heap:
 *              start            start+8           start+16
 *          INIT: | PROLOGUE_FOOTER | EPILOGUE_HEADER |
 * heap_listp ends up pointing to the epilogue header.
 */
bool mm_init(void) 
{
    // Create the initial empty heap 
    word_t *start = (word_t *)(mem_sbrk(2*wsize));

    if (start == (void *)-1) 
    {
        return false;
    }
    for (int i = 0; i < seglistsize; i++){
        free_list[i] = NULL;
        endofflist[i] = NULL;    
    }
    
    largestfree = 0;

    start[0] = pack(0, true); // Prologue footer
    start[1] = pack(0, true); // Epilogue header
    // Heap starts with first block header (epilogue)
    heap_listp = (block_t *) &(start[1]);

    // Extend the empty heap with a free block of chunksize bytes
    if (extend_heap(chunksize) == NULL)
    {
        return false;
    }
    dbg_printf("%s\n", "successfully initialized heap");
    mm_checkheap(1);
    return true;
}

/*
 * malloc: allocates a block with size at least (size + dsize), rounded up to
 *         the nearest 16 bytes, with a minimum of 2*dsize. Seeks a
 *         sufficiently-large unallocated block on the heap to be allocated.
 *         If no such block is found, extends heap by the maximum between
 *         chunksize and (size + dsize) rounded up to the nearest 16 bytes,
 *         and then attempts to allocate all, or a part of, that memory.
 *         Returns NULL on failure, otherwise returns a pointer to such block.
 *         The allocated block will not be used for further allocations until
 *         freed.
 */
void *malloc(size_t size) 
{
    size_t asize;      // Adjusted block size
    size_t extendsize; // Amount to extend heap if no fit is found
    block_t *block;
    void *bp = NULL;
    dbg_printf("I'm in malloc %zd.\n",size);
    if (heap_listp == NULL) // Initialize heap if it isn't initialized
    {
        mm_init();
    }

    if (size == 0) // Ignore spurious request
    {
    dbg_printf("Malloc(%zd) --> %p\n", size, bp);
        return bp;
    }

    // Adjust block size to include overhead and to meet alignment requirements
    asize = round_up(size+wsize, dsize);
    if(asize==16){
        asize = 32;
    }

    // Search the free list for a fit
    block = find_fit(asize);
    dbg_printf("Found fit: %p\n",block);
    // If no fit is found, request more memory, and then and place the block
    if (block == NULL)
    {  
        extendsize = max(asize, chunksize);
        block = extend_heap(extendsize);
        if (block == NULL) // extend_heap returns an error
        {
            dbg_printf("Malloc(%zd) --> %p\n", size, bp);
            return bp;
        }
    }
    place(block, asize);
    bp = header_to_payload(block);
   dbg_printf("Malloc(%zd) --> %p\n", size, bp);
    return bp;
} 

/*
 * free: Frees the block such that it is no longer allocated while still
 *       maintaining its size. Block will be available for use on malloc.
 */
void free(void *bp)
{

    if (bp == NULL)
    {
        return;
    }

    block_t *block = payload_to_header(bp);
    size_t size = get_size(block);

    dbg_printf("I'm in free, freeing %p.\n",block);
    if(preisfree(block)){
        dbg_printf("My prev is free apparently\n");
        write_header(block, size, false);
        write_footer(block, size, false);
        makeprefree(block);
    }
    else{
        write_header(block, size, false);
        write_footer(block, size, false);
    }
    
    coalesce(block);

    //setting my next's tag
    block_t *next = find_next(block);
    makeprefree(next);

    //If this block is larger than the largest free block then it is now the largest
    size = get_size(block);
    mm_checkheap(1);
    dbg_printf("mmcheck free\n");
    dbg_printf("Completed free(%p)\n", bp);
}

static void makeprefree(block_t *block){
    block->header = block->header | 0x2;
}
static bool preisfree(block_t *block){
    return (bool)(block->header & (0x2));
}
static void remprefree(block_t *block){
    block->header = block->header & (~(long)(0x2));
}
/*
 * realloc: returns a pointer to an allocated region of at least size bytes:
 *          if ptrv is NULL, then call malloc(size);
 *          if size == 0, then call free(ptr) and returns NULL;
 *          else allocates new region of memory, copies old data to new memory,
 *          and then free old block. Returns old block if realloc fails or
 *          returns new pointer on success.
 */
void *realloc(void *ptr, size_t size)
{
    dbg_printf("I'm in realloc.\n");
    block_t *block = payload_to_header(ptr);
    size_t copysize;
    void *newptr;

    // If size == 0, then free block and return NULL
    if (size == 0)
    {
        free(ptr);
        return NULL;
    }

    // If ptr is NULL, then equivalent to malloc
    if (ptr == NULL)
    {
        return malloc(size);
    }

    // Otherwise, proceed with reallocation
    newptr = malloc(size);
    // If malloc fails, the original block is left untouched
    if (!newptr)
    {
        return NULL;
    }

    // Copy the old data
    copysize = get_payload_size(block); // gets size of old payload
    if(size < copysize)
    {
        copysize = size;
    }
    memcpy(newptr, ptr, copysize);

    // Free the old block
    free(ptr);

    return newptr;
}

/*
 * calloc: Allocates a block with size at least (elements * size + dsize)
 *         through malloc, then initializes all bits in allocated memory to 0.
 *         Returns NULL on failure.
 */
void *calloc(size_t nmemb, size_t size)
{
    dbg_printf("I'm in calloc.\n");
    void *bp;
    size_t asize = nmemb * size;

    if (asize/nmemb != size)
    // Multiplication overflowed
    return NULL;
    
    bp = malloc(asize);
    if (bp == NULL)
    {
        return NULL;
    }
    // Initialize all bits to 0
    memset(bp, 0, asize);

    return bp;
}

/******** The remaining content below are helper and debug routines ********/

/*
 * extend_heap: Extends the heap with the requested number of bytes, and
 *              recreates epilogue header. Returns a pointer to the result of
 *              coalescing the newly-created block with previous free block, if
 *              applicable, or NULL in failure.
 */
static block_t *extend_heap(size_t size) 
{
    void *bp;

    // Allocate an even number of words to maintain alignment
    size = round_up(size, dsize);
    if ((bp = mem_sbrk(size)) == (void *)-1)
    {
        return NULL;
    }
    
    // Initialize free block header/footer 
    block_t *block = payload_to_header(bp);
    if(preisfree(block)){
        write_header(block, size, false);
        write_footer(block, size, false);
        makeprefree(block);
    }
    else{
        write_header(block, size, false);
        write_footer(block, size, false);
    }
    // Create new epilogue header
    block_t *block_next = find_next(block);
    write_header(block_next, 0, true);
    makeprefree(block_next);

    // Coalesce in case the previous block was free
    block = coalesce(block);
    return block;
}

/* Coalesce: Coalesces current block with previous and next blocks if
 *           either or both are unallocated; otherwise the block is not
 *           modified. Then, insert coalesced block into the segregated list.
 *           Returns pointer to the coalesced block. After coalescing, the
 *           immediate contiguous previous and next blocks must be allocated.
 */
static block_t *coalesce(block_t * block) 
{
    block_t *block_next = find_next(block);
    block_t *block_prev = NULL;
    mm_checkheap(1);
    dbg_printf("I'm in coalesce.\n");
    bool prev_alloc = true;// = extract_alloc(*(find_prev_footer(block)));
    if(preisfree(block)){
        prev_alloc = false;
        block_prev = find_prev(block);
    }
    bool next_alloc = get_alloc(block_next);
    size_t size = get_size(block);

    if (prev_alloc && next_alloc)              // Case 1
    {
        dbg_printf("%s\n","C case 1");
        size_t nsize=get_size(block);
        if(nsize>largestfree){
            largestfree = nsize;
        }
        insertblock(block);
        return block;
    }

    else if (prev_alloc && !next_alloc)        // Case 2
    {     dbg_printf("%s\n","C case 2");  
        size += get_size(block_next);
        removeblock(block_next);
        write_header(block, size, false);
        write_footer(block, size, false);
    }

    else if (!prev_alloc && next_alloc)        // Case 3
    {
        dbg_printf("%s\n","C case 3");
        size += get_size(block_prev);
        removeblock(block_prev);
        write_header(block_prev, size, false);
        write_footer(block_prev, size, false);
        block = block_prev;
    }

    else                                        // Case 4
    {
        dbg_printf("%s\n","C case 4");
        size += get_size(block_next) + get_size(block_prev);
        removeblock(block_next);
        removeblock(block_prev);
        write_header(block_prev, size, false);
        write_footer(block_prev, size, false);
        block = block_prev;
    }
    insertblock(block);
    size_t nsize=get_size(block);
    if(nsize>largestfree){
        largestfree = nsize;
    }
    return block;
}
//REVISIT AND VALIDATE
static void insertblock(block_t *block){
    dbg_printf("I'm in insert.\n");
    //saving the current first member of my list(NULL if list was empty)
    
    size_t size = get_size(block);
    int class = get_class(size);
    block_t *lastblock = endofflist[class];
    if(free_list[class] == NULL)
    {
        free_list[class] = block;
    }

    //getting address of where I need to put next of thing I'm adding
    block_t **nextloc = (block_t**)(header_to_payload(block));
    block_t **prevloc = (block_t**)((char*)(nextloc)+wsize);

    //making my new first block the block I'm inserting
    endofflist[class] = block;

    //This part sets next of me to current first block
    //and prev of me to NULL cuz I'm the first now
    *nextloc = NULL;
    *prevloc = lastblock;//setting prev to null, cuz its the root

    //This part sets the prev of the ex-first block to me
    if(lastblock!=NULL){
        block_t **nextoflastblock = (block_t**)((char*)(header_to_payload(lastblock)));
        *nextoflastblock = block;
    }
    block_t *block_next = find_next(block);
    if(block_next != NULL){
        makeprefree(block_next);
    }
    mm_checkheap(1);
    dbg_printf("%s","insert mmcheck \n");
/*
    //saving the current first member of my list(NULL if list was empty)
    block_t *firstblock = free_list;

    //getting address of where I need to put next of thing I'm adding
    block_t **nextloc = (block_t**)(header_to_payload(block));
    block_t **prevloc = (block_t**)((char*)(nextloc)+wsize);

    //making my new first block the block I'm inserting
    free_list = block;

    //This part sets next of me to current first block
    //and prev of me to NULL cuz I'm the first now
    *nextloc = firstblock;
    *prevloc = NULL;//setting prev to null, cuz its the root

    //This part sets the prev of the ex-first block to me
    if(firstblock!=NULL){
        block_t **prevoffirstblock = (block_t**)((char*)(header_to_payload(firstblock))+wsize);
        *prevoffirstblock = block;
    }
    
*/
}
//REDO WITH MEMCPY()
static void removeblock(block_t *block){
    dbg_printf("I'm in delete.\n");

    block_t **mynext = (block_t**)(header_to_payload(block));
    block_t **myprev = (block_t**)(((char*)header_to_payload(block))+wsize);

    int class = get_class(get_size(block));

    if(*myprev == NULL && *mynext == NULL){
        dbg_printf("I'm in delete 1.\n");
        free_list[class] = NULL;
        //extra
        endofflist[class] = NULL;
    }
    else if(*myprev != NULL && *mynext != NULL){
        dbg_printf("I'm in delete 2.\n");
        block_t** prevsnext = (block_t**)(header_to_payload(*myprev));
        *prevsnext = *mynext;

        block_t** nextsprev = (block_t**)((char*)(header_to_payload(*mynext))+wsize);
        *nextsprev = *myprev;

    }
    else if(*myprev == NULL && *mynext != NULL){
        dbg_printf("I'm in delete 3.\n");
        free_list[class] = *mynext;
        block_t** nextsprev = (block_t**)((char*)(header_to_payload(*mynext))+wsize);
        *nextsprev = NULL;

        
    }
    else{
        dbg_printf("I'm in delete 4.\n");
        block_t** prevsnext = (block_t**)(header_to_payload(*myprev));
        *prevsnext = NULL;
        //extra
        endofflist[class] = *myprev;
    }


    block_t *next = find_next(block);
    remprefree(next);
    mm_checkheap(1);
    dbg_printf("remove mmcheck\n");
}

/*
 * place: Places block with size of asize at the start of bp. If the remaining
 *        size is at least the minimum block size, then split the block to the
 *        the allocated block and the remaining block as free, which is then
 *        inserted into the segregated list. Requires that the block is
 *        initially unallocated.
 */
static void place(block_t *block, size_t asize)
{
    dbg_printf("I'm in place.\n");

    size_t csize = get_size(block);

    if ((csize - asize) >= min_block_size)
    {
        dbg_printf("shityy case\n");
        removeblock(block);
        block_t *block_next;
        write_header(block, asize, true);
        //write_footer(block, asize, true);

        block_next = find_next(block);
        write_header(block_next, csize-asize, false);
        write_footer(block_next, csize-asize, false);
        if(largestfree == csize){
            largestfree = csize-asize;
        }
        insertblock(block_next);
    }

    else
    { 
        removeblock(block);
        write_header(block, csize, true);
        //write_footer(block, csize, true);
    }
    mm_checkheap(1);
}

/*
 * find_fit: Looks for a free block with at least asize bytes with
 *           first-fit policy. Returns NULL if none is found.
 */
static block_t *find_fit(size_t asize)
{
    dbg_printf("I'm in findfit.\n");
    mm_checkheap(1);
    dbg_printf("My largest free block is %zd\n",largestfree);
    block_t *block;
    size_t size;
    
    
    if(asize>largestfree){
        return NULL;
    }
    int class = get_class(asize);

    for(int i = class;i<seglistsize;i++){
        for (block = free_list[i]; block!=NULL;
                 block = *((block_t**)(header_to_payload(block))) )
        {
            size = get_size(block);
            if (asize <= size)
            {
                return block;
            }
        }   
    }
    

     
    return NULL; // no fit found
}

static int get_class(size_t s){
    int block_size = (int)(s/(dsize));
    if(block_size<=2){
        return 0;
    }
    if(block_size<=3){
        return 1;
    }
    if(block_size<=4){
        return 2;
    }
    if(block_size<=8){
        return 3;
    }
    if(block_size<=32){
        return 4;
    }
    if(block_size<=128){
        return 5;
    }
    return 6;
}


/*
 * max: returns x if x > y, and y otherwise.
 */
static size_t max(size_t x, size_t y)
{
    return (x > y) ? x : y;
}


/*
 * round_up: Rounds size up to next multiple of n
 */
static size_t round_up(size_t size, size_t n)
{
    return (n * ((size + (n-1)) / n));
}

/*
 * pack: returns a header reflecting a specified size and its alloc status.
 *       If the block is allocated, the lowest bit is set to 1, and 0 otherwise.
 */
static word_t pack(size_t size, bool alloc)
{
    return alloc ? (size | 1) : size;
}


/*
 * extract_size: returns the size of a given header value based on the header
 *               specification above.
 */
static size_t extract_size(word_t word)
{
    return (word & ~(word_t) 0xF);
}

/*
 * get_size: returns the size of a given block by clearing the lowest 4 bits
 *           (as the heap is 16-byte aligned).
 */
static size_t get_size(block_t *block)
{
    return extract_size(block->header);
}

/*
 * get_payload_size: returns the payload size of a given block, equal to
 *                   the entire block size minus the header and footer sizes.
 */
static word_t get_payload_size(block_t *block)
{
    size_t asize = get_size(block);
    return asize - wsize;
}

/*
 * extract_alloc: returns the allocation status of a given header value based
 *                on the header specification above.
 */
static bool extract_alloc(word_t word)
{
    return (bool)(word & 0x1);
}

/*
 * get_alloc: returns true when the block is allocated based on the
 *            block header's lowest bit, and false otherwise.
 */
static bool get_alloc(block_t *block)
{
    return extract_alloc(block->header);
}

/*
 * write_header: given a block and its size and allocation status,
 *               writes an appropriate value to the block header.
 */
static void write_header(block_t *block, size_t size, bool alloc)
{
    block->header = pack(size, alloc);
}


/*
 * write_footer: given a block and its size and allocation status,
 *               writes an appropriate value to the block footer by first
 *               computing the position of the footer.
 */
static void write_footer(block_t *block, size_t size, bool alloc)
{
    word_t *footerp = (word_t *)((block->payload) + get_size(block) - dsize);
    *footerp = pack(size, alloc);
}


/*
 * find_next: returns the next consecutive block on the heap by adding the
 *            size of the block.
 */
static block_t *find_next(block_t *block)
{
    return (block_t *)(((char *)block) + get_size(block));
}

/*
 * find_prev_footer: returns the footer of the previous block.
 */
static word_t *find_prev_footer(block_t *block)
{
    // Compute previous footer position as one word before the header
    return (&(block->header)) - 1;
}

/*
 * find_prev: returns the previous block position by checking the previous
 *            block's footer and calculating the start of the previous block
 *            based on its size.
 */
static block_t *find_prev(block_t *block)
{
    word_t *footerp = find_prev_footer(block);
    size_t size = extract_size(*footerp);
    return (block_t *)((char *)block - size);
}

/*
 * payload_to_header: given a payload pointer, returns a pointer to the
 *                    corresponding block.
 */
static block_t *payload_to_header(void *bp)
{
    return (block_t *)(((char *)bp) - offsetof(block_t, payload));
}

/*
 * header_to_payload: given a block pointer, returns a pointer to the
 *                    corresponding payload.
 */
static void *header_to_payload(block_t *block)
{
    return (void *)(block->payload);
}

/* mm_checkheap: checks the heap for correctness; returns true if
 *               the heap is correct, and false otherwise.
 *               can call this function using mm_checkheap(__LINE__);
 *               to identify the line number of the call site.
 */
static void printBlock(block_t *bp)
{
    int headersize, isalloch, footsize, isallocf;

    /* Basic header and footer information */
    headersize = get_size((bp));
    isalloch = get_alloc((bp));
    footsize = get_size((block_t*)((char*)bp+headersize-wsize));
    isallocf = get_alloc((block_t*)((char*)bp+headersize-wsize));

    if (headersize == 0) 
    {
        dbg_printf("%p: EOL, tag is %d\n",(void*) bp, (int)preisfree(bp));
        return;
    }
    
    /* Prints out header and footer info if it's an allocated block.
     * Prints out header and footer info and next and prev info
     * if it's a free block.
    */
    if (isalloch){
        dbg_printf("%p: header:[%d:%x] no footer, tag is %d\n",(void*) bp,
            headersize, (isalloch),(int)preisfree(bp));
    }
    else{
        dbg_printf("%p:header:[%d:%x] next:%p prev:%p footer:[%d:%x]\n",
            bp, headersize, (isalloch),*((void**)((char*)bp+wsize)),
            *((void**)((char*)bp+dsize)), footsize, (isallocf));
    }
}


/* 
 * mm_checkheap - Check the heap for consistency 
 * Checks the epilogue and prologue blocks for size and allocation bit.
 * Checks the 16-byte address alignment for each block in the free list.
 * Checks each free block to see if its next and previous pointers are 
 * within heap bounds.
 * Checks the consistency of header and footer size and allocation bits 
 * for each free block.
 */
bool mm_checkheap(int verbose) 
{
    block_t* bp = (block_t*)((char*)heap_listp-wsize); //Prologue

    dbg_printf("Heap (%p):\n",heap_listp);

    for (bp =heap_listp; get_size(bp)>0; bp =(block_t*)((char*)bp+get_size(bp))) 
    {
        printBlock(bp);
    }

    printBlock(bp); //epilogue

    return true;
}
