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

public class Points {
    public static class Pair<A, B> {
        public final A first;
        public final B second;

        public Pair(final A first, final B second) {
            this.first = first;
            this.second = second;
        }
        //
        // Override 'equals', 'hashcode' and 'toString'
        //
        public String toString() {
            return this.first.toString()+","+this.second.toString();
        }
    }

    
    
    public static double distance(Pair<Double,Double> a, Pair<Double,Double> b) {
            return Math.sqrt(Math.pow(a.first-b.first,2)+Math.pow(a.second-b.second,2));
    }

    public static Pair<Double,Double> addpoints(Pair<Double,Double> a, Pair<Double,Double> b) {
        return new Pair<Double,Double>(a.first+b.first,a.second+b.second);
    }

    public static class PointsMapper extends MapReduceBase
                            implements Mapper<LongWritable, Text, IntWritable, Text> {       
        public ArrayList<Pair<Double,Double>> centroids = new ArrayList<Pair<Double,Double>>();           
        public void configure(JobConf j) {
            // Initially before mapper runs, I want to get all the centroids off a file I conveniently stored them in
            
            try {
                Configuration config = new Configuration();
                FileSystem hdfs = FileSystem.get((new Path("/user/hadoop/points/clusters.txt")).toUri(), config);
                BufferedReader br = new BufferedReader(new InputStreamReader(hdfs.open(new Path("/user/hadoop/points/clusters.txt"))));
                String line = br.readLine();
                while(line!=null) {
                    String[] xy = line.split(",");
                    Pair<Double,Double> centroidi = new Pair<Double,Double>(Double.parseDouble(xy[0]),Double.parseDouble(xy[1]));
                    centroids.add(centroidi);
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

            // First, I wanna parse this value into a point. Then I wanna find which centroid its closest to from the list. 
            String p = value.toString();
            String[] xy = p.split(",");
            Pair<Double,Double> point = new Pair<Double,Double>(Double.parseDouble(xy[0]),Double.parseDouble(xy[1]));
            double min = distance(point,centroids.get(0));
            int mindex = 0;
            double cur;
            for(int i = 0; i < centroids.size(); i++) {
                cur = distance(point,centroids.get(i));
                if(cur<min) {
                    min = cur;
                    mindex = i;
                }
            }
            output.collect(new IntWritable(mindex),value);                   
        }
    }
    
    public static class PointsReducer extends MapReduceBase 
                        implements Reducer<IntWritable, Text, Text, Text> {
                            
        public void reduce (IntWritable key, Iterator<Text> values,
                                OutputCollector<Text, Text> output,
                                Reporter report) throws IOException  {
                
            // Reduce logic: sum up all the points of this cluster and divide by cluster size and cool stuff like that
            // This gives us the true mean, which is our new centroid post this iteration
            System.out.println("Updating key: "+Integer.toString(key.get()));
            Pair<Double,Double> sum = new Pair<Double,Double>(0.0,0.0);
            int ctr = 0;
            while (values.hasNext()){
                String[] xy = values.next().toString().split(",");
                Pair<Double,Double> thispoint = new Pair<Double,Double>(Double.parseDouble(xy[0]),Double.parseDouble(xy[1])); 
                sum = addpoints(sum,thispoint);
                ctr++;
            }
            Pair<Double,Double> centroid = new Pair<Double,Double>(sum.first/ctr,sum.second/ctr);
            String out = Double.toString(centroid.first) + "," + Double.toString(centroid.second);
            output.collect(new Text(Integer.toString(key.get())+" "+Integer.toString(ctr)), new Text(out));

        }
    }

    // Checks for convergence, with a threshhold of 0.05
    public static boolean converged(ArrayList<String> old, ArrayList<String> now) {
        for(int i = 0; i < old.size(); i++) {
            String[] oldi = old.get(i).split(",");
            String[] newi = now.get(i).split(",");
            double oldx = Double.parseDouble(oldi[0]);
            double oldy = Double.parseDouble(oldi[1]);
            double newx = Double.parseDouble(newi[0]);
            double newy = Double.parseDouble(newi[1]);
            if(distance(new Pair<Double,Double>(oldx,oldy),new Pair<Double, Double>(newx,newy))>0.05) {
                return false;
            }
        }
        return true;
    }

    public static void main (String args[]) throws Exception {
        
        JobConf conf = new JobConf(Points.class);

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

        // Now access the points from hdfs (better ways pls suggest ;-;)
        ArrayList<Pair<Double,Double>> centroids = new ArrayList<Pair<Double,Double>>();
        Configuration config = new Configuration();
        FileSystem hdfs = FileSystem.get((new Path(args[0])).toUri(), config);
        BufferedReader br = new BufferedReader(new InputStreamReader(hdfs.open(new Path(inputloc))));
        BufferedWriter wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/points/clusters.txt"))));
        int tracker = 0;
        for(int i = 0; i < numpoints; i++) {
            String curpoint = br.readLine();
            if(tracker>=clusters) {
                break;
            }
            if(locations.get(tracker) == i) {
                wr.write(curpoint+"\n");
                String[] xy = curpoint.split(",");
                double x = Double.parseDouble(xy[0]);
                double y = Double.parseDouble(xy[1]);
                Pair<Double,Double> pointi = new Pair<Double,Double>(x,y);
                centroids.add(pointi);
                tracker++;
            }
        }
        wr.close();
        br.close();

        // Now that we have an initial set of points, we're ready to run the job!
        int runtime = 1;
        conf.setJobName("Points iteration: "+Integer.toString(runtime));
        
        conf.setMapperClass(PointsMapper.class);
        conf.setReducerClass(PointsReducer.class);

        conf.setMapOutputKeyClass(IntWritable.class);
        conf.setMapOutputValueClass(Text.class);
        conf.setOutputKeyClass(Text.class);
        conf.setOutputValueClass(Text.class);
        conf.set("mapred.textoutputformat.separator", ",");
        JobClient.runJob(conf);

        
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
            br = new BufferedReader(new InputStreamReader(hdfs.open(new Path("/user/hadoop/points/clusters.txt"))));
            for(int i = 0; i < clusters; i++) {
                String oldi = br.readLine();
                oldclusters.add(oldi);
            }
            br.close();
            
            // As I read from output and write into clusters, I memorize the values
            String[] centroidlist = new String[clusters];
            ArrayList<String> curclusters = new ArrayList<String>();
            String[] numPoints = new String[clusters];
            hdfs.delete(new Path("/user/hadoop/points/clusters.txt"), true);
            wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/points/clusters.txt"))));
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

            hdfs.delete(out, true);
            conf = new JobConf(Points.class);
            conf.setJobName("Points iteration: "+Integer.toString(runtime));
        
            conf.setMapperClass(PointsMapper.class);
            conf.setReducerClass(PointsReducer.class);

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
        hdfs.delete(new Path("/user/hadoop/points/clusters.txt"), true);
        wr = new BufferedWriter(new OutputStreamWriter(hdfs.create(new Path("/user/hadoop/points/clusters.txt"))));
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
    }
}