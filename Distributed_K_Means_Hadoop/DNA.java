import java.util.*;
import java.io.*;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.PathFilter;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.util.*;

public class DNA {
    
    // Finds the distance between two DNA strands in terms of number of different corresponding base pairs
    public static double distance(String a, String b) {
        int dis = 0;
        for(int i = 0; i < a.length(); i++) {
            if(a.charAt(i)!=b.charAt(i)) {
                dis++;
            }
        }
        return dis;
    }

    public static class DNAMapper extends MapReduceBase
                            implements Mapper<LongWritable, Text, IntWritable, Text> {       
        public ArrayList<String> centroids = new ArrayList<String>();           
        public void configure(JobConf j) {
            // Initially before mapper runs, I want to get all the centroids off a file I conveniently stored them in
            
            try {
                Configuration config = new Configuration();
                FileSystem hdfs = FileSystem.get((new Path("/user/hadoop/DNA/clusters.txt")).toUri(), config);
                BufferedReader br = new BufferedReader(new InputStreamReader(hdfs.open(new Path("/user/hadoop/DNA/clusters.txt"))));
                String line = br.readLine();
                while(line!=null) {
                    centroids.add(line);
                    line = br.readLine();
                }
                br.close();
            } catch (IOException e) {
                //not happening 
            }
            

        }
        public void map (LongWritable key, Text value, 
                            OutputCollector<IntWritable, Text> output,
                            Reporter report) throws IOException {

            // I wanna find which centroid its closest to from the list. 
            String p = value.toString();
            double min = distance(p,centroids.get(0));
            int mindex = 0;
            double cur;
            for(int i = 0; i < centroids.size(); i++) {
                cur = distance(p,centroids.get(i));
                if(cur<min) {
                    min = cur;
                    mindex = i;
                }
            }
            // Forward blindly keeping the centroid id as key
            output.collect(new IntWritable(mindex),value);                   
        }
    }
    
    public static class DNAReducer extends MapReduceBase 
                        implements Reducer<IntWritable, Text, Text, Text> {
                            
        public void reduce (IntWritable key, Iterator<Text> values,
                                OutputCollector<Text, Text> output,
                                Reporter report) throws IOException  {
                
            // Reduce logic: find the true mean (in this case mode)
            // This is our new centroid post this iteration
            String line = values.next().toString();
            int[] sumA = new int[line.length()];
            int[] sumT = new int[line.length()];
            int[] sumG = new int[line.length()];
            int[] sumC = new int[line.length()];
            for(int i = 0; i < sumA.length; i++) {
                if(line.charAt(i) == 'A') {
                    sumA[i]++;
                } else if (line.charAt(i) == 'T') {
                    sumT[i]++;
                } else if (line.charAt(i) == 'G') {
                    sumG[i]++;
                } else if (line.charAt(i) == 'C'){
                    sumC[i]++;
                }
            }
            int ctr = 1;
            while (values.hasNext()){
                line = values.next().toString();
                for(int i = 0; i < sumA.length; i++) {
                    if(line.charAt(i) == 'A') {
                        sumA[i]++;
                    } else if (line.charAt(i) == 'T') {
                        sumT[i]++;
                    } else if (line.charAt(i) == 'G') {
                        sumG[i]++;
                    } else if (line.charAt(i) == 'C'){
                        sumC[i]++;
                    }
                }
                ctr++;
            }
            String centroid = "";
            for(int i = 0; i < line.length(); i++) {
                if(sumA[i]>=sumT[i] && sumA[i]>=sumG[i] && sumA[i]>=sumC[i]) {
                    centroid+="A";
                } else if(sumT[i]>=sumA[i] && sumT[i]>=sumG[i] && sumT[i]>=sumC[i]) {
                    centroid+="T";
                } else if(sumG[i]>=sumA[i] && sumG[i]>=sumT[i] && sumG[i]>=sumC[i]) {
                    centroid+="G";
                } else {
                    centroid+="C";
                }
            }
            output.collect(new Text(Integer.toString(key.get())+" "+Integer.toString(ctr)), new Text(centroid));

        }
    }


    // Checks if values converged
    public static boolean converged(ArrayList<String> old, ArrayList<String> now) {
        for(int i = 0; i < old.size(); i++) {
            String oldi = old.get(i);
            String newi = now.get(i);
            if(distance(oldi,newi)>0) {
                return false;
            }
        }
        return true;
    }

    public static void main (String args[]) throws Exception {
        
        JobConf conf = new JobConf(DNA.class);

        // Reading all the given inputs
        String inputloc = args[0];
        Path out = new Path(args[1]);
        FileInputFormat.setInputPaths(conf, new Path(args[0]));
        FileOutputFormat.setOutputPath(conf, out);
        int numpoints = Integer.parseInt(args[2]);
        int clusters = Integer.parseInt(args[3]);
        int iterations = Integer.parseInt(args[4]);
        
        // Need to collect the initial centroids from the input file
        // Start by generating centroid number of random dudes(unique tho!)
        Random generator = new Random(System.currentTimeMillis());
        ArrayList<Integer> locations = new ArrayList<Integer>();
        int nextloc = 0;
        for(int i = 0; i < clusters; i++) {
            nextloc = generator.nextInt(numpoints);
            while(locations.contains(nextloc)) {
                nextloc = generator.nextInt(numpoints);
            }
            locations.add(nextloc);
        }
        Collections.sort(locations);

        // Now get the points from the list of all points as we read over them linearly
        ArrayList<String> centroids = new ArrayList<String>();
        Configuration config = new Configuration();
        FileSystem hdfs = FileSystem.get((new Path(args[0])).toUri(), config);
        BufferedReader br = new BufferedReader(new InputStreamReader(hdfs.open(new Path(inputloc))));
        BufferedWriter wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/DNA/clusters.txt"))));
        int tracker = 0;
        for(int i = 0; i < numpoints; i++) {
            String curpoint = br.readLine();
            if(tracker>=clusters) {
                break;
            }
            if(locations.get(tracker) == i) {
                wr.write(curpoint+"\n");
                centroids.add(curpoint);
                tracker++;
            }
        }
        wr.close();
        br.close();

        // Now that we have an initial set of centroids, we're ready to run the job!
        int runtime = 1;
        conf.setJobName("DNA iteration: "+Integer.toString(runtime));
        
        conf.setMapperClass(DNAMapper.class);
        conf.setReducerClass(DNAReducer.class);

        conf.setMapOutputKeyClass(IntWritable.class);
        conf.setMapOutputValueClass(Text.class);
        conf.setOutputKeyClass(Text.class);
        conf.setOutputValueClass(Text.class);
        conf.set("mapred.textoutputformat.separator", ",");
        JobClient.runJob(conf);

        // Keeps running job iteration times or till convergence
        runtime+=1;
        while(runtime <= iterations || iterations == 0){
            // Grab the output and set it up for next use: write them to centroids
            PathFilter filter = new PathFilter () {
                public boolean accept(Path file) {
                    return file.getName().startsWith("part-");
                }
            };
            FileStatus[] outfiles = hdfs.listStatus(out,filter);
            
            // Collecting the old clusters so I can check for convergence
            ArrayList<String> oldclusters = new ArrayList<String>();
            br = new BufferedReader(new InputStreamReader(hdfs.open(new Path("/user/hadoop/DNA/clusters.txt"))));
            for(int i = 0; i < clusters; i++) {
                String oldi = br.readLine();
                oldclusters.add(oldi);
            }
            br.close();
            
            // As I read from output and write into clusters, I memorize the values
            String[] centroidlist = new String[clusters];
            ArrayList<String> curclusters = new ArrayList<String>();
            String[] numPoints = new String[clusters];
            hdfs.delete(new Path("/user/hadoop/DNA/clusters.txt"), true);
            wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/DNA/clusters.txt"))));
            for(int i = 0; i < outfiles.length; i++){
                br = new BufferedReader(new InputStreamReader(hdfs.open(outfiles[i].getPath())));
                String line = br.readLine();
                while(line!=null) {
                    String l2 = line.substring(line.indexOf(",")+1);
                    String[] info = line.substring(0,line.indexOf(",")).split(" ");
                    centroidlist[Integer.parseInt(info[0])] = l2;
                    numPoints[Integer.parseInt(info[0])] = info[1];
                    wr.write(l2+"\n",0,l2.length()+1);
                    line = br.readLine();
                }
                br.close();
            }
            for(int i = 0; i < clusters; i++) {
                curclusters.add(centroidlist[i]);
            }
            wr.close();


            // This is the point where I see if I've converged, if I have, I need to break
            if(converged(oldclusters,curclusters)){
                System.out.println("Converged in: "+Integer.toString(runtime)+" runs!");
                wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path(out,"centroids"))));
                for(int i = 0; i < curclusters.size(); i++) {
                    wr.write(curclusters.get(i)+","+numPoints[i]+"\n",0,curclusters.get(i).length()+numPoints[i].length()+2);
                }
                wr.close();
                return;
            }


            // Running the job again
            hdfs.delete(out, true);
            conf = new JobConf(DNA.class);
            conf.setJobName("DNA iteration: "+Integer.toString(runtime));
        
            conf.setMapperClass(DNAMapper.class);
            conf.setReducerClass(DNAReducer.class);

            conf.setMapOutputKeyClass(IntWritable.class);
            conf.setMapOutputValueClass(Text.class);
            conf.setOutputKeyClass(Text.class);
            conf.setOutputValueClass(Text.class);
            FileInputFormat.setInputPaths(conf, new Path(args[0]));
            FileOutputFormat.setOutputPath(conf, out);
            conf.set("mapred.textoutputformat.separator", ",");
            JobClient.runJob(conf);
            runtime+=1;

        }

        // Just writing from the set of output files to a single output file- "centroids"
        // in the output directory with the given format in mind

        // Getting MapReduce outfiles
        PathFilter filter = new PathFilter () {
            public boolean accept(Path file) {
                return file.getName().startsWith("part-");
            }
        };
        FileStatus[] outfiles = hdfs.listStatus(out,filter);
            
        // As I read from output and write into clusters, I memorize the values
        String[] centroidlist = new String[clusters];
        ArrayList<String> curclusters = new ArrayList<String>();
        String[] numPoints = new String[clusters];
        hdfs.delete(new Path("/user/hadoop/DNA/clusters.txt"), true);
        wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/DNA/clusters.txt"))));
        for(int i = 0; i < outfiles.length; i++){
            br = new BufferedReader(new InputStreamReader(hdfs.open(outfiles[i].getPath())));
            String line = br.readLine();
            while(line!=null) {
                String l2 = line.substring(line.indexOf(",")+1);
                String[] info = line.substring(0,line.indexOf(",")).split(" ");
                centroidlist[Integer.parseInt(info[0])] = l2;
                numPoints[Integer.parseInt(info[0])] = info[1];
                wr.write(l2+"\n",0,l2.length()+1);
                line = br.readLine();
            }
            br.close();
        }
        for(int i = 0; i < clusters; i++) {
            curclusters.add(centroidlist[i]);
        }
        wr.close();
        wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path(out,"centroids"))));
        for(int i = 0; i < curclusters.size(); i++) {
            wr.write(curclusters.get(i)+","+numPoints[i]+"\n",0,curclusters.get(i).length()+numPoints[i].length()+2);
        }
        wr.close();
        // Completed
    }
}