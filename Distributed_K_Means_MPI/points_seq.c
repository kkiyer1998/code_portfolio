#include <getopt.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>

typedef struct
{
    float x;
    float y;
} point;



void printUsage() {
    printf("%s\n", "Input format for points_seq.c: ");
    printf("%s\n", "-c <arg>: Number of clusters c");
    printf("%s\n", "-t <arg>: Total number of points provided");
    printf("%s\n", "-i <arg>: Input file containing points");
    printf("%s\n", "-n [arg]: Number of iterations, will stop at 5 default.");
}

float distance(point p, point q) {
    return (float)sqrt(pow((double)(p.x-q.x),2) + pow((double)(p.y-q.y),2));
}

bool aresimilar(point x, point* set, int size) {
    int i;
    for(i = 0; i < size; i++){
        if(fabs(x.x - set[i].x)<0.05)
            return true;
        if(fabs(x.y - set[i].y)<0.05)
            return true;
    }
    return false;
}

int main( int argc, char** argv ) {
    /* Initializing Variables */
    FILE* f;
    char* buffer;
    // input values
    int clusters = 0;
    int points = 0;
    char *inpfile;
    int iterations = -1;
    // flag for validating inputs
    int fl;
    // my iterators
    int i;
    int j;
    int k;
    /* */

    
    /* Parsing arguments as appropriate */ 
    while((fl = getopt(argc, argv, "c:t:i:n:")) != -1){
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

    point* datapoints = malloc(points*sizeof(point));
    for(i = 0; i < points; i++){
        float a;
        float b;
        if(fscanf(f, "%e,%e\n", &a, &b) != 2){
            printf("%s\n", "Invalid file format");
            return 0;
        }
        datapoints[i].x = a;
        datapoints[i].y = b;
    }
    /* */


    srand(time(0));
    /* Setting initial values to the k clusters */
    point* centroids = malloc(clusters*sizeof(point));
    for(i = 0; i < clusters; i++) {
        int index = rand() % points;
        while(aresimilar(datapoints[index],centroids,clusters)) {
            index = rand() % points;
        }
        centroids[i].x = datapoints[index].x;
        centroids[i].y = datapoints[index].y;
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
            float min = distance(centroids[0],datapoints[j]);
            my_centroid[j] = 0;
            for(k = 1; k < clusters; k++){
                float dis = distance(centroids[k],datapoints[j]);
                if(dis<min) {
                    min = dis;
                    my_centroid[j] = k;
                }
            }
        }

        // Now the list mycentroid maps indices in datapoints to their linked centroids.
        // Our next step is finding the mean for each cluster
        for(j = 0; j < clusters; j++){
            float sumx = 0;
            float sumy = 0;
            int count = 0;

            // Iterate through all points and find the ones of this cluster. Add their 
            // values to a maintained sum, and increment a maintained counter.
            for(k = 0; k < points; k++){
                // this means this point is in my cluster
                if(my_centroid[k] == j) {
                    sumx += datapoints[k].x;
                    sumy += datapoints[k].y;
                    count++;
                }
            }

            if(count == 0) {
                printf("No points in this mean... What's it even doing here?\n");
                return 0;
            }

            // Finally, we compute the true mean of the data points, and make these means
            // the centroids
            float averagex = sumx/(float)count;
            float averagey = sumy/(float)count;
            if(fabs(centroids[j].x-averagex)>0.05 || fabs(centroids[j].y-averagey)>0.05)
                flag = true;
            centroids[j].x = averagex;
            centroids[j].y = averagey;
        }

        // iterating
        if(i>=0)
            i++;

        // If flag remains false through the iteration, then the values have NOT changed much: convergence.
    }

    printf("Centroid List: \n");
    for(j = 0; j < clusters; j++) {
        printf("Centroid %d: %f,%f\n", j, centroids[j].x, centroids[j].y); 
    }
    char* outputfile = "points_seq_results";
    FILE* out = fopen(outputfile,"w");
    fprintf(out, "%s\n", "Centroid List:");
    for(j = 0; j < clusters; j++) {
        fprintf(out, "Centroid %d: %f,%f\n", j, centroids[j].x, centroids[j].y); 
    }
    fclose(out);
    /* Free the shit that gotta be freed son */
    free(centroids);
    free(datapoints);
    free(my_centroid);
}