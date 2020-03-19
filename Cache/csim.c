#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <strings.h>
#include <math.h>
#include <unistd.h>
#include "cachelab.h"

typedef unsigned long long address;

/* 
The structs in which I hold the cache and its vaious parts:
    cache_Info- carries all the details about size
                (Also keeps track of hits, misses, evicts)
    cache- Carries an array of cache_sets
    cache_set- Carries an array of cache_lines
    cache_line- Has an LRU counter, and the line info.
*/

struct cache_Info{
    int E;
    int b;
    int s;
    int no_sets;
    int block_size;
    int hits;
    int misses;
    int evicts;
};

struct cache_line{
    int used;
    int validbit;
    address tag;
    char* block;
};

struct cache_set{
    struct cache_line* lines;
};

struct cache{
    struct cache_set *sets;
};


/* initcache 
 * This function loads the cache onto memory using the malloc library
 * This is not trivial because I load each set and each line
 * It returns a cache 
 */
struct cache initcache(struct cache_Info info){
    //initializing a cache of s sets
    struct cache newcache;
    newcache.sets = 
    (struct cache_set*) malloc(sizeof(struct cache_set)*info.no_sets);
    /* This loop will initialize each line of each set in newcache */
    for (int i = 0; i < info.no_sets; i++)
    {
        newcache.sets[i].lines = 
        (struct cache_line*)malloc(sizeof(struct cache_line)*info.E);
        for (int j = 0; j < info.E; j++)
        {
            newcache.sets[i].lines[j].validbit = 0;
            newcache.sets[i].lines[j].tag = 0;
            newcache.sets[i].lines[j].used = 0;
        }
    }
    return newcache;
}


/* clearcache
 * This is my free function that helps me 
 * free the cache that I loaded through malloc
 */
void clearcache(struct cache oldcache,struct cache_Info info)
{
    for(int i = 0; i < info.no_sets; i++)
    {
        if(oldcache.sets[i].lines!=NULL)
            free(oldcache.sets[i].lines);
    }
    if(oldcache.sets!=NULL)
        free(oldcache.sets);
}


/* findminmax
 * This is the main LRU algorithm that I learnt from the internet
 * It finds the max and the min of the used of the lines of a set.
 * This is so I can evict the minused and set the new loaded block to
 * maxused+1. This helps me keep the oldest one to throw and the newest to keep
 */
int findminmax(struct cache_line *curset,struct cache_Info info,int* minmax)
{
    minmax[0] = curset[0].used;
    minmax[1] = curset[0].used;
    int oldest_line=0;
    for(int line=1;line<info.E;line++)
    {
        if(curset[line].used<minmax[0]){
            oldest_line = line;
            minmax[0] = curset[line].used;
        }
        if(curset[line].used>minmax[1]){
            minmax[1] = curset[line].used;
        }
    }
    return oldest_line;
}

/* operate 
 * This function actually does the main cache operations
 * We call this for both load and stores since their effect on the hits
 * would be the same. (We dont really care about the actual block of info)
 * This function finds the set and looks for a tag among the lines in the set
 * If it misses and the set is full then we evict using LRU, else we just
 * load it into the empty block we found. 
 */
struct cache_Info operate(struct cache main_cache,struct cache_Info info,address memloc)
{
    int set_full = 1;
    int tagsize = (64-(info.s+info.b));
    address tag = (memloc>>(info.s+info.b));
    address set = ((memloc<<tagsize)>>(tagsize+info.b));
    struct cache_line* curset = main_cache.sets[set].lines;
    int hit = 0;
    int emptyspot = 0;

    for(int line = 0; line < info.E; line++)
    {
        struct cache_line curline = curset[line];
        if(curline.validbit)
        {
            if(curline.tag == tag)
            {
                hit = 1;
                curline.used++;
                info.hits++;
                curset[line] = curline;
                printf("%s\n", "Hit");
            }
        }
        else if(set_full){
            set_full = 0;
            emptyspot = line;
        }
    }
    if(hit){
        return info;
    }
    info.misses++;
    int* minmax = malloc(sizeof(int)*2);
    int oldest_line = findminmax(curset,info,minmax);

    if(set_full){
        printf("%s\n", "Evict");
        info.evicts++;    
        curset[oldest_line].tag = tag;
        curset[oldest_line].used = minmax[1]+1;
    }
    else{
        curset[emptyspot].tag = tag;
        curset[emptyspot].validbit = 1;
        curset[emptyspot].used = minmax[1]+1;
    }
    if (minmax!=NULL){
        free(minmax);
    }
    return info;
}

//printUsage- Prints instructions on how to run file
void printUsage()
{
    printf("Flags with their implications for this executable:\n");
    printf("-h                 Usage instruction flag.\n");
    printf("-s <signed bits>   No. of set bits.\n");
    printf("-b <offset bits>   No of. block bits.\n");
    printf("-E <No. of lines>  No. of lines in every set.\n");
    return;
}

/* Main function, collects arguments, reads the file and 
 * accordingly loads to the cache. It prints out the final misses, hits and evicts
 */
int main(int argc, char **argv)
{
    /* code */
    struct cache_Info info;
    //int vflag;
    char* tracefile;
    int c;

    //Parsing option args:
    while ((c = getopt (argc, argv, "vs:E:b:t:")) != -1)
        switch (c)
        {
            //case 'v':
                //vflag = 1;
                //break;
            case 'h':
                printUsage();
                return 0;
            case 's':
                info.s = atoi(optarg);
                break;
            case 'E':
                info.E = atoi(optarg);
                break;
            case 'b':
                info.b = atoi(optarg);
                break;
            case 't':
                tracefile = optarg;
                break;
            default:
                abort ();
        }


    /* I will now initialize my cache */
    info.no_sets = pow(2.0,info.s);
    info.block_size = pow(2.0,info.b);
    info.hits = 0;
    info.misses = 0;
    info.evicts = 0;
    struct cache main_cache = initcache(info);

    /* Now I begin to read the file containing the reads and writes from and 
    to memory via my cache */
    FILE *trace = fopen(tracefile,"r");
    char instr;
    address memloc;
    int size;
    if(trace == NULL){
        fprintf(stderr, "%s\n", "Bad file name... Git Gud :/");
        clearcache(main_cache,info);
        return 0;
    }
    while(fscanf(trace," %c %llx,%x",&instr,&memloc,&size)==3){
        switch(instr){
            case 'L':
            case 'S':
                info = operate(main_cache,info,memloc);
                break;
            default:
                break;
        }
    }
    printSummary(info.hits,info.misses,info.evicts);
    clearcache(main_cache,info);
    return 0;
}