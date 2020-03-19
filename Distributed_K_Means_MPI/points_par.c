#include <mpi.h>
#include <getopt.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>


#define ROOT 0


typedef struct {
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

int main(int argc, char **argv) {
    
    int num_procs;
    int my_pid;
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
    // The content of the operations
    point* datapoints = malloc(points*sizeof(point));
    point* recvpoints = malloc(points*sizeof(point));
    point* centroids = malloc(clusters*sizeof(point));
    
    MPI_Init(&argc, &argv);
    
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_pid);

    

    MPI_Status status;
    double start_time;

    // Creating a new data type for a point
    MPI_Datatype mpi_point;
    MPI_Type_contiguous(2, MPI_FLOAT, &mpi_point);

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

    // Root process initializes array - reads from file, and creates centroids
    if (my_pid == ROOT) {
        FILE* f;
        /* Need to parse input file to get points */
        f = fopen(inpfile, "r");
        if(f == NULL) {
            printf("%s\n", "File Does not exist. Aborting...");
            return 0;
        }


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
        for(i = 0; i < clusters; i++) {
            int index = rand() % points;
            while(aresimilar(datapoints[index],centroids,clusters)) {
                index = rand() % points;
            }
            centroids[i].x = datapoints[index].x;
            centroids[i].y = datapoints[index].y;
        }
        /* */
        start_time = MPI_Wtime();

        // Tag 1: Centroids
        for(i = 0; i < num_procs; i++) {
            MPI_Send(centroids, clusters, mpi_point, i, 1, MPI_COMM_WORLD);
        }
        
    } else {
        MPI_Recv(centroids, clusters, mpi_point, ROOT, 1, MPI_COMM_WORLD, &status);

    }

    int num_elem = points;
    int elem_per_proc = num_elem/num_procs;

    // Scatter the points  
    MPI_Scatter(datapoints, elem_per_proc, mpi_point, datapoints, elem_per_proc, mpi_point, ROOT, MPI_COMM_WORLD);

    
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
        for(j = 0; j < elem_per_proc; j++) {
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
            for(k = 0; k < elem_per_proc; k++){
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


        // Recombining to get the new centroids.
        if(my_pid != ROOT) {
            MPI_Send(centroids, clusters, mpi_point, i, 2, MPI_COMM_WORLD);
            MPI_Recv(centroids, clusters, mpi_point, ROOT, 3, MPI_COMM_WORLD, &status);
        } else {
            point* resulti = malloc(clusters*sizeof(point));
            for(j = 1; j < num_procs; j++){
                MPI_Recv(resulti, clusters, mpi_point, ROOT, 2, MPI_COMM_WORLD, &status);
                for(k = 0; k < clusters; k++) {
                    centroids[k].x+= resulti[k].x;
                    centroids[k].y+= resulti[k].y;
                }
            }
            for(j = 0; j < clusters; j++) {
                centroids[j].x = centroids[j].x/num_procs;
                centroids[j].y = centroids[j].y/num_procs;
            }

            for(i = 1; i < num_procs; i++) {
                MPI_Send(centroids, clusters, mpi_point, i, 3, MPI_COMM_WORLD);
            }
        }



        // If flag remains false through the iteration, then the values have NOT changed much: convergence.
    }
    
    
    // Print time and sum from the root
    if (my_pid == ROOT) {
        double end_time = MPI_Wtime();
        printf("Centroid List: \n");
        for(j = 0; j < clusters; j++) {
            printf("Centroid %d: %f,%f\n", j, centroids[j].x, centroids[j].y); 
        }

    }
    
    MPI_Finalize();

}