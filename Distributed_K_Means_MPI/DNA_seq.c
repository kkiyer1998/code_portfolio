#include <getopt.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    char* bases;
    int len;
} strand;



void printUsage() {
    printf("%s\n", "Input format for points_seq.c: ");
    printf("%s\n", "-c <arg>: Number of clusters c");
    printf("%s\n", "-t <arg>: Total number of points provided");
    printf("%s\n", "-i <arg>: Input file containing points");
    printf("%s\n", "-n [arg]: Number of iterations, will stop at 5 default.");
    printf("%s\n", "-l <arg>: Length of strands");
}

float distance(strand p, strand q) {
    int i;
    int d = 0;
    for(i = 0; i < p.len; i++){
        if(p.bases[i]!=q.bases[i]){
            d++;
        }
    }
    return d;
}

void printStrand(strand x) {
    int i;
    for(i = 0; i < x.len; i++){
        printf("%c", x.bases[i]);
    }
}

bool aresimilar(strand x, strand* set, int size) {
    int i;
    for(i = 0; i < size; i++){
        if(distance(set[i],x) < 5){
            return true;
        }
    }
    return false;
}

int main( int argc, char** argv ) {
    /* Initializing Variables */
    FILE* f;
    // input values
    int clusters = 0;
    int points = 0;
    char *inpfile;
    int iterations = -1;
    int strandLen = 0;
    // flag for validating inputs
    int fl;
    // my iterators
    int i;
    int j;
    int k;
    /* */

    
    /* Parsing arguments as appropriate */ 
    while((fl = getopt(argc, argv, "c:t:i:n:l:")) != -1){
        switch(fl) {
            case 'c':
                clusters = atoi(optarg);
                break;
            case 't':
                points = atoi(optarg);
                break;
            case 'i':
                inpfile = optarg;
                break;
            case 'n':
                iterations = atoi(optarg);
                break;
            case 'l':
                strandLen = atoi(optarg);
            case '?':
                break;
            default:
                printUsage();
                return 0;
        }
    }

    if(clusters == 0 || points == 0 || inpfile == NULL){
        printUsage();
        return 0;
    }
    /* */

    /* Need to parse input file to get points */
    f = fopen(inpfile, "r");
    if(f == NULL) {
        printf("%s\n", "File Does not exist. Aborting...");
        return 0;
    }

    strand* strands = malloc(points*sizeof(strand));
    for(i = 0; i < points; i++){
        strands[i].len = strandLen;
        strands[i].bases = malloc(strandLen*sizeof(char));
    }

    for(i = 0; i < points; i++){
        fgets(strands[i].bases, strandLen+1, f);
        if(fgetc(f) != '\n') {
            printf("Invalid File format.\n");
            return 0;
        }
    }
    /* */

    srand(time(0));
    /* Setting initial values to the k clusters */
    strand* centroids = malloc(clusters*sizeof(strand));
    for(i = 0; i < clusters; i++) {
        centroids[i].len = strandLen;
        centroids[i].bases = malloc(strandLen*sizeof(char));
    }
    
    for(i = 0; i < clusters; i++) {
        int index = rand() % points;
        while(aresimilar(strands[index], centroids, i))
            index = rand() % points;
        int a;
        for(a = 0; a < strandLen; a++){
            centroids[i].bases[a] = strands[index].bases[a];
        }
    }
    /* */

    



    /* Start the k-means algorithm: 
        For each point, I find the centroid it is closest to, and add it to
        that one's array.

        Each cluster will have its own array of points. We calculate the means 
        of each and make these the new centroids. 
        We repeat this entire procedure 'iterations' number of times.
    */
    // This flag will be false on convergence
    bool flag = true;
    // this array links point i to its corresponding centroid position.
    int* my_centroid = malloc(points*sizeof(int));
    if(iterations == -1)
        i = -2;
    else
        i = 0;
    while (i < iterations && flag){
        flag = false;
        // iterating over all points
        for(j = 0; j < points; j++) {
            // finding closest of all centroids
            int min = distance(centroids[0],strands[j]);
            my_centroid[j] = 0;
            for(k = 1; k < clusters; k++){
                int dis = distance(centroids[k],strands[j]);
                if(dis<min) {
                    min = dis;
                    my_centroid[j] = k;
                }
            }
        }

        // Now the list mycentroid maps indices in strands to their linked centroids.
        // Our next step is finding the mean for each cluster
        for(j = 0; j < clusters; j++){
            int sumA[strandLen];
            int sumT[strandLen];
            int sumG[strandLen];
            int sumC[strandLen];
            int count = 0;

            // Iterate through all points and find the ones of this cluster. Add their 
            // values to a maintained sum, and increment a maintained counter.
            for(k = 0; k < points; k++){
                // this means this point is in my cluster
                if(my_centroid[k] == j) {
                    int a;
                    for(a = 0; a < strandLen; a++){
                        switch(strands[k].bases[a]){
                            case 'A':
                                sumA[a]+= 1;
                                break;
                            case 'T':
                                sumT[a]+= 1;
                                break;
                            case 'G':
                                sumG[a]+= 1;
                                break;
                            case 'C':
                                sumC[a]+= 1;
                                break;
                            default:
                                printf("Impossible, exiting\n");
                                return 0;
                        }
                    }
                    count++;
                }
            }

            if(count == 0) {
                printf("No points in this mean... What's it even doing here?\n");
                return 0;
            }

            // Finally, we compute the true mean of the data points, and make these means
            // the centroids
            strand newmean;
            newmean.bases = malloc(strandLen*sizeof(char));
            newmean.len = strandLen;
            int a;
            for(a = 0; a < strandLen; a++){
                if(sumA[a]>sumT[a] && sumA[a]>sumC[a] && sumA[a]>sumG[a])
                    newmean.bases[a] = 'A';
                else if(sumT[a]>sumA[a] && sumT[a]>sumC[a] && sumT[a]>sumG[a])
                    newmean.bases[a] = 'T';
                else if(sumG[a]>sumA[a] && sumG[a]>sumT[a] && sumG[a]>sumC[a])
                    newmean.bases[a] = 'G';
                else
                    newmean.bases[a] = 'C';
            }
            
            if(distance(newmean, centroids[j])<2)
                flag = true;
            for(a = 0; a < strandLen; a++){
                centroids[j].bases[a] = newmean.bases[a];
            }
            free(newmean.bases);
        }

        // iterating
        if(i>=0)
            i++;

        // If flag remains false through the iteration, then the values have NOT changed much: convergence.
    }


    char* outputfile = "DNA_seq_results";
    FILE* out = fopen(outputfile, "w");
    fprintf(out, "Centroid List: \n");
    for(j = 0; j < clusters; j++) {
        char basej[strandLen];
        for(i = 0; i < strandLen; i++){
            basej[i] = centroids[j].bases[i];
        }
        basej[i] = '\0';
        fprintf(out, "Centroid %d: %s\n", j, basej); 
    }




    /* Free the shit that gotta be freed son */
    for(i = 0; i < clusters; i++) {
        free(centroids[i].bases);
    }
    free(centroids);
    for(i = 0; i < points; i++) {
        free(strands[i].bases);
    }
    free(strands);
    free(my_centroid);
}