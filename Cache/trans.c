/* 
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * All matrix sizes are handled in seperate conditional blocks within 
 * transpose_submit. 
 * 
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */ 
#include <stdio.h>
#include "cachelab.h"
#include "contracts.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/* 
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. The REQUIRES and ENSURES from 15-122 are included
 *     for your convenience. They can be removed if you like.
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    int rowblock,colblock,blocksize;
    REQUIRES(M > 0);
    REQUIRES(N > 0);
    if(M==61){
        int tmp,index;
        /* For matrices of size 61X67 */
        blocksize = 8;
        //This loop handles the transposing
        for (rowblock = 0; rowblock < N; rowblock+=blocksize)
        {
            for (colblock = 0; colblock < M; colblock+=blocksize)
            {
                //Iteration within a block
                for(int c = colblock; (c < M) && (c < colblock+blocksize); c++)
                {
                    for(int r = rowblock; (r < N) && (r < rowblock+blocksize); r++)
                    {
                        //Diagonal elements stored in a temp variable because they 
                        //cause more evicts
                        if(r!=c){
                            B[c][r]= A[r][c];
                        }
                        else{
                            tmp = A[r][c];
                            index = r;
                        }
                    }
                    //Putting the right diagonal element in B
                    if (rowblock==colblock)
                    {
                        B[index][index] = tmp;
                    }
                }
            }
        }
        return;
    }
    if(M==32){
        int tmp,index;
        /* For matrices of size 32X32 */
        blocksize = 8;
        for (rowblock = 0; rowblock < N; rowblock+=blocksize)
        {
            for (colblock = 0; colblock < M; colblock+=blocksize)
            {
                //Iterating through a block
                for(int r = rowblock; (r < N) && (r < rowblock+blocksize); r++)
                {
                    for(int c = colblock; (c < M) && (c < colblock+blocksize); c++)
                    {
                        //Diag elements stored in a temp variable to avoid evicts
                        if(r!=c){
                            B[c][r]= A[r][c];
                        }
                        else{
                            tmp = A[r][c];
                            index = r;
                        }
                    }
                    //Setting diag of B to the temp
                    if (rowblock==colblock)
                    {
                        B[index][index] = tmp;
                    }
                }
            }
        }
        return;
    }
    /* Code for Transposing 64X64 */
    int v1,v2,v3,v4,v5,v6,v7,v8;
    for (rowblock = 0; rowblock < N; rowblock+=4)
    {
        for (colblock = 0; colblock < M; colblock+=4)
        {
            //Within a single block
            for(int r = rowblock; r < rowblock+4; r+=2)
            {
                //This stores all the members of the first two 
                //rows in a block, to avoid some *extra* misses
                v1 = A[r][colblock];
                v2 = A[r][colblock+1];
                v3 = A[r][colblock+2];
                v4 = A[r][colblock+3];
                v5 = A[r+1][colblock];
                v6 = A[r+1][colblock+1];
                v7 = A[r+1][colblock+2];
                v8 = A[r+1][colblock+3];
                B[colblock][r] = v1;
                B[colblock+1][r] = v2;
                B[colblock+2][r] = v3;
                B[colblock+3][r] = v4;
                B[colblock][r+1] = v5;
                B[colblock+1][r+1] = v6;
                B[colblock+2][r+1] = v7;
                B[colblock+3][r+1] = v8;
            }
        }
    }
    ENSURES(is_transpose(M, N, A, B));
}

/* 
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started. 
 */ 

/* 
 * trans - A simple baseline transpose function, not optimized for the cache.
 */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;

    REQUIRES(M > 0);
    REQUIRES(N > 0);

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }    

    ENSURES(is_transpose(M, N, A, B));
}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc); 

    /* Register any additional transpose functions */
    registerTransFunction(trans, trans_desc); 

}

/* 
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

