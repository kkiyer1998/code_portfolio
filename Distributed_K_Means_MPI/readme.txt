The sequential implementations of DNA and points are given, and the parallel implementation of points (incompelete)

running the kmeans.sh produces two files cluster.csv and clusterDNA.csv that can be used for the points and DNA execution respectively 

compile points_seq.c with:

> gcc -lm points_seq.c

then run with:

> ./a.out -c *Number of clusters* -t *total number of points* -i *input file* -n *Number of iterations(default: till convergence)*

compile DNA_seq.c with:

> gcc -lm DNA_seq.c

then run with:

> ./a.out -c *Number of clusters* -t *total number of points* -i *input file* -n *Number of iterations(default: till convergence)* -l *Strand Length*

Results are posted on files DNA_seq_results and points_seq_results.

