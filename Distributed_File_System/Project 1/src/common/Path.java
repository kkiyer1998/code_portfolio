package common;

import java.io.*;
import java.util.*;

import storage.Command;

/** Distributed filesystem paths.

    <p>
    Objects of type <code>Path</code> are used by all filesystem interfaces.
    Path objects are immutable.

    <p>
    The string representation of paths is a forward-slash-delimeted sequence of
    path components. The root directory is represented as a single forward
    slash.

    <p>
    The colon (<code>:</code>) and forward slash (<code>/</code>) characters are
    not permitted within path components. The forward slash is the delimeter,
    and the colon is reserved as a delimeter for application use.
 */
public class Path implements Iterable<String>, Serializable, Comparable<Path>
{
	/*
	 * Data members:
	 * 
	 * ArrayList<String> path:
	 * the data structure representing the path to a directory.
	 * 
	 */
	ArrayList<String> path;
	
	/*
	 * Constructor declarations:
	 */
	
	
    /** Creates a new path which represents the root directory. */
    public Path()
    {
        path = new ArrayList<String>();
    }

    /** Creates a new path by appending the given component to an existing path.

        @param path The existing path.
        @param component The new component.
        @throws IllegalArgumentException If <code>component</code> includes the
                                         separator, a colon, or
                                         <code>component</code> is the empty
                                         string.
    */
    public Path(Path path, String component)
    {
    	if(component.length()==0){
    		throw new IllegalArgumentException("Empty component");
    	}
    	this.path = new ArrayList<String>();
    	for(int i = 0; i < path.path.size(); i++){
    		this.path.add(path.path.get(i));
    	}
    	for(int i=0; i < component.length(); i++){
    		if(component.charAt(i) == ','
    				|| component.charAt(i) == ':'
    				|| component.charAt(i) == '/'){
    			throw new IllegalArgumentException("Bad component");
    		}
    	} 
    	this.path.add(component);
    }

    /** Creates a new path from a path string.

        <p>
        The string is a sequence of components delimited with forward slashes.
        Empty components are dropped. The string must begin with a forward
        slash.

        @param path The path string.
        @throws IllegalArgumentException If the path string does not begin with
                                         a forward slash, or if the path
                                         contains a colon character.
     */
    public Path(String path)
    {
    	this.path = new ArrayList<String>();
    	if(path == null || path.length()==0 || path.charAt(0)!='/'){
    		throw new IllegalArgumentException("Invalid Path input");
    	}
    	String current = "";
    	for(int i=1; i < path.length(); i++){
    		if(path.charAt(i)==':'){
    			throw new IllegalArgumentException("Path input contains \':\'");
    		} else if(path.charAt(i)=='/'){
    			if(current.length()>0){
    				this.path.add(current);
    			}
    			current = "";
    		} else {
    			current = current + Character.toString(path.charAt(i));
    		}
    	}
    	if(current.length()>0){
    		this.path.add(current);
    	}
        
    }
    
    /*
     * End of constructors, beginning of class methods.
     */

    /** Returns an iterator over the components of the path.

        <p>
        The iterator cannot be used to modify the path object - the
        <code>remove</code> method is not supported.

        @return The iterator.
     */
    @Override
    public Iterator<String> iterator()
    {
        PathIterator<String> x = new PathIterator<String>();
        return x;
    }
    
    
    /*
     * This class implements the iterator that is forwarded to the client
     * It goes down the path by a directory at each iteration.
     */
    private class PathIterator<x> implements Iterator<x>{
    	
		@Override
		public boolean hasNext() {
			if(path.size()>0){
				return true;
			}
			return false;
		}

		@SuppressWarnings("unchecked")
		@Override
		public x next() {
			if(path.size()==0){
				throw new NoSuchElementException("Path exhausted");
			}
			String result = path.get(0);
			ArrayList<String> path1 = new ArrayList<String>();
			for(int i = 1; i < path.size(); i++){
				path1.add(path.get(i));
			}
			path = path1;
			return (x)result;
		}
    	
    }

    /** Lists the paths of all files in a directory tree on the local
        filesystem.

        @param directory The root directory of the directory tree.
        @return An array of relative paths, one for each file in the directory
                tree.
        @throws FileNotFoundException If the root directory does not exist.
        @throws IllegalArgumentException If <code>directory</code> exists but
                                         does not refer to a directory.
     */
	public static Path[] list(File directory) throws FileNotFoundException
    {
    	if(directory == null || !directory.exists()){
    		throw new IllegalArgumentException("Bad root.");
    	}
    	if(!directory.isDirectory()){
    		throw new FileNotFoundException("Root is not a valid directory");
    	}
    	Path myroot = new Path();
    	ArrayList<Path> files = new ArrayList<Path>();
    	File[] x = directory.listFiles();
    	for(int i = 0; i < x.length; i++){
    		if(x[i].isFile()){
    			Path newfile = new Path(myroot,x[i].getName());
    			files.add(newfile);
    		} else {
    			Path[] mylist = myroot.list(x[i]);
    			for(int k = 0; k < mylist.length; k++){
    				files.add(new Path("/"+x[i].getName()+"/"+mylist[k].toString()));
    			}
    		}
    	}
    	Path[] a = new Path[files.size()];
    	files.toArray(a);
    	return a;
    }

    /** Determines whether the path represents the root directory.

        @return <code>true</code> if the path does represent the root directory,
                and <code>false</code> if it does not.
     */
    public boolean isRoot()
    {
    	return this.path.isEmpty();
    }

    /** Returns the path to the parent of this path.

        @throws IllegalArgumentException If the path represents the root
                                         directory, and therefore has no parent.
     */
    public Path parent()
    {
        if(this.isRoot()){
        	throw new IllegalArgumentException("Root has no parent");
        }
        String newpath = "/";
        for(int i = 0; i < this.path.size()-1; i++){
        	newpath = newpath + this.path.get(i) + "/";
        }
        newpath = newpath.substring(0, newpath.length()-1);
        if(newpath.length()==0){
        	//root
        	newpath = "/";
        }
        return new Path(newpath);
    }

    /** Returns the last component in the path.

        @throws IllegalArgumentException If the path represents the root
                                         directory, and therefore has no last
                                         component.
     */
    public String last()
    {
    	if(this.isRoot()){
    		throw new IllegalArgumentException("Root doesnt have a last!");
    	}
        return this.path.get(this.path.size()-1);
    }

    /** Determines if the given path is a subpath of this path.

        <p>
        The other path is a subpath of this path if is a prefix of this path.
        Note that by this definition, each path is a subpath of itself.

        @param other The path to be tested.
        @return <code>true</code> If and only if the other path is a subpath of
                this path.
     */
    public boolean isSubpath(Path other)
    {
    	if(other.path.size()>this.path.size()){
    		return false;
    	}
    	for(int i = 0; i < other.path.size(); i++){
    		if(!(other.path.get(i)).equals(this.path.get(i))){
    			return false;
    		}
    	}
    	return true;
    }

    /** Converts the path to <code>File</code> object.

        @param root The resulting <code>File</code> object is created relative
                    to this directory.
        @return The <code>File</code> object.
     */
    public File toFile(File root)
    {
        return new File(root,this.toString());
    }

    /** Compares two paths for equality.

        <p>
        Two paths are equal if they share all the same components.

        @param other The other path.
        @return <code>true</code> if and only if the two paths are equal.
     */
    @Override
    public boolean equals(Object other)
    {
    	if(!other.getClass().equals(this.getClass())){
    		return false;
    	}
    	if(((Path)(other)).path.size() != this.path.size()){
    		return false;
    	}
    	for(int i = 0; i < ((Path)(other)).path.size(); i++){
    		if(!(((Path)(other)).path.get(i)).equals(this.path.get(i))){
    			return false;
    		}
    	}
    	return true;
    }

    /** Returns the hash code of the path. */
    @Override
    public int hashCode()
    {
    	int depth = this.path.size();
    	String last = this.path.get(depth-1);
    	int sum = 0;
    	for(int i = 0; i < last.length(); i++){
    		char x = (last.charAt(i));
    		sum+= Character.hashCode(x);
    	}
    	return depth + sum;
    }

    /** Converts the path to a string.

        <p>
        The string may later be used as an argument to the
        <code>Path(String)</code> constructor.

        @return The string representation of the path.
     */
    @Override
    public String toString()
    {
    	String newpath = "/";
    	if(this.path.size()==0){
    		return newpath;
    	}
        for(int i = 0; i < this.path.size(); i++){
        	newpath = newpath + this.path.get(i) + "/";
        }
        return newpath.substring(0, newpath.length()-1);
    }

	    /** Compares this path to another.
	
	    <p>
	    An ordering upon <code>Path</code> objects is provided to prevent
	    deadlocks between applications that need to lock multiple filesystem
	    objects simultaneously. By convention, paths that need to be locked
	    simultaneously are locked in increasing order.
	
	    <p>
	    Because locking a path requires locking every component along the path,
	    the order is not arbitrary. For example, suppose the paths were ordered
	    first by length, so that <code>/etc</code> precedes
	    <code>/bin/cat</code>, which precedes <code>/etc/dfs/conf.txt</code>.
	
	    <p>
	    Now, suppose two users are running two applications, such as two
	    instances of <code>cp</code>. One needs to work with <code>/etc</code>
	    and <code>/bin/cat</code>, and the other with <code>/bin/cat</code> and
	    <code>/etc/dfs/conf.txt</code>.
	
	    <p>
	    Then, if both applications follow the convention and lock paths in
	    increasing order, the following situation can occur: the first
	    application locks <code>/etc</code>. The second application locks
	    <code>/bin/cat</code>. The first application tries to lock
	    <code>/bin/cat</code> also, but gets blocked because the second
	    application holds the lock. Now, the second application tries to lock
	    <code>/etc/dfs/conf.txt</code>, and also gets blocked, because it would
	    need to acquire the lock for <code>/etc</code> to do so. The two
	    applications are now deadlocked.
	
	    <p>
	    As a general rule to prevent this scenario, the ordering is chosen so
	    that objects that are near each other in the path hierarchy are also
	    near each other in the ordering. That is, in the above example, there is
	    not an object such as <code>/bin/cat</code> between two objects that are
	    both under <code>/etc</code>.
	
	    @param other The other path.
	    @return Zero if the two paths are equal, a negative number if this path
	            precedes the other path, or a positive number if this path
	            follows the other path.
	 */
    @Override
    public int compareTo(Path other)
    {
    	int i = 0;
    	while(true){
    		String mydir = this.path.get(i);
    		String otherdir = this.path.get(i);
    		if(mydir.compareTo(otherdir)<0){
    			return -1;
    		} else if (mydir.compareTo(otherdir)>0){
    			return 1;
    		} else {
    			i+=1;
    			if(i == mydir.length() || i == otherdir.length()){
    				if(mydir.length() == otherdir.length()){
    					return 0;
    				} else {
    					return mydir.length()-otherdir.length();
    				}
    			}
    		}
    	}
    	
    }
}
