package naming;

import java.io.*;
import java.net.InetSocketAddress;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import rmi.*;
import common.*;
import storage.*;

/** Naming server.

    <p>
    Each instance of the filesystem is centered on a single naming server. The
    naming server maintains the filesystem directory tree. It does not store any
    file data - this is done by separate storage servers. The primary purpose of
    the naming server is to map each file name (path) to the storage server
    which hosts the file's contents.

    <p>
    The naming server provides two interfaces, <code>Service</code> and
    <code>Registration</code>, which are accessible through RMI. Storage servers
    use the <code>Registration</code> interface to inform the naming server of
    their existence. Clients use the <code>Service</code> interface to perform
    most filesystem operations. The documentation accompanying these interfaces
    provides details on the methods supported.

    <p>
    Stubs for accessing the naming server must typically be created by directly
    specifying the remote network address. To make this possible, the client and
    registration interfaces are available at well-known ports defined in
    <code>NamingStubs</code>.
 */
public class NamingServer implements Service, Registration
{
	/*
	 * Data members of naming server:
	 */
	public Skeleton<Registration> register;
	public Skeleton<Service> service;
	public InetSocketAddress sport;
	public InetSocketAddress rport;
	public ArrayList<Command> commands;
	public ArrayList<Storage> stubs;
	public Branch dirTree;
	/** Default naming server client service port. */
    public static final int     SERVICE_PORT = 6000;
    /** Default naming server registration port. */
    public static final int     REGISTRATION_PORT = 6001;
	

    /*
     * Structures used to implement the directory tree and lock mechanism,
     * with toString impl for testing purposes.
     */
    public class Lock {
    	boolean exclusive;
    	//has a list of the people waiting for locks, 
    	//true for exclusive, false for shared
    	ArrayList<SimpleEntry<Boolean, Integer>> waiters;
    	public int id = 0;
    	
    	public Lock(){
    		waiters = new ArrayList<>();
    		exclusive = false;
    	}
    	
    	public int enq(boolean x){
    		waiters.add(new SimpleEntry<Boolean, Integer>(x,id));
    		return id++;
    	}
    	
    	public boolean deq() throws IOException{
    		if(waiters.size()==0){
    			throw new IOException("invalid input");
    		}
    		boolean x = waiters.get(0).getKey().booleanValue();
    		waiters.remove(0);
    		return x;
    	}
    	
    	public boolean peek(){
    		return waiters.get(0).getKey();
    	}
    	
    	

    }
	public class Node {
		String name;
		//contains info about the lock on this node
		public Lock lock;
		public int numReaders;
		public final int WRITE = -1;
		
		public Node(String n){
			name = n;
			numReaders = 0;
			lock = new Lock();
		}
		
		public boolean hasWrite(int id){
			ArrayList<SimpleEntry<Boolean, Integer>> l = lock.waiters;
			for(int i = 0; i < l.size(); i++){
				if(l.get(i).getKey().booleanValue()){
					if(lock.waiters.get(i).getValue() < id)
						return true;
				}
			}
			return false;
		}
		
		public boolean isTop(int id){
			return lock.waiters.get(0).getValue() == id;
		}
		
	}
	public class Branch extends Node {
		ArrayList<Node> list;
		public Branch(String n, ArrayList<Node> x) {
			super(n);
			list = x;
		}
		public String toString(){
			String fin = this.name+": ";
			for(int i = 0; i < list.size(); i++){
				fin = fin + list.get(i).toString();
			}
			return " ("+fin+") ";
		}
	}
	public class Leaf extends Node {
		Command c;
		Storage s;
		ArrayList<Command> replicas;
		int numRequests;
		public Leaf(String n){
			super(n);
		}
		public Leaf(String n, Command c, Storage s) {
			super(n);
			this.replicas = new ArrayList<>();
			this.c = c;
			this.s = s;
			numRequests = 0;
		}
		public String toString(){
			return "\""+this.name + "\" ";
		}
	}
	
	/*
	 * End of data member declarations, 
	 * beginning of classs functions.
	 * 
	 */
	
    /** Creates the naming server object.

        <p>
        The naming server is not started.
     */
    public NamingServer()
    {
        dirTree = new Branch("/",new ArrayList<Node>());
        commands = new ArrayList<Command>();
        stubs = new ArrayList<Storage>();
        sport = new InetSocketAddress(SERVICE_PORT);
        rport = new InetSocketAddress(REGISTRATION_PORT);
        register = new Skeleton<Registration>(Registration.class,this,rport);
        service = new Skeleton<Service>(Service.class,this,sport);
    }

    /** Starts the naming server.

        <p>
        After this method is called, it is possible to access the client and
        registration interfaces of the naming server remotely.

        @throws RMIException If either of the two skeletons, for the client or
                             registration server interfaces, could not be
                             started. The user should not attempt to start the
                             server again if an exception occurs.
     */
    public synchronized void start() throws RMIException
    {
        try {
        	register.start();
        	service.start();
        } catch(Exception e){
        	throw new RMIException("Failed to start naming server");
        }
    }

    /** Stops the naming server.

        <p>
        This method waits for both the client and registration interface
        skeletons to stop. It attempts to interrupt as many of the threads that
        are executing naming server code as possible. After this method is
        called, the naming server is no longer accessible remotely. The naming
        server should not be restarted.
     */
    public void stop()
    {
        register.stop();
        service.stop();
        stopped(null);
    }

    /** Indicates that the server has completely shut down.

        <p>
        This method should be overridden for error reporting and application
        exit purposes. The default implementation does nothing.

        @param cause The cause for the shutdown, or <code>null</code> if the
                     shutdown was by explicit user request.
     */
    protected void stopped(Throwable cause)
    {
    }

    
    
    
    
    
    // The following methods are documented in Service.java.
    @Override
    public synchronized void lock(Path path, boolean exclusive)
            throws RMIException, FileNotFoundException {
    	
    	try{
    		this.isDirectory(path);
    	} catch (FileNotFoundException e){
    		throw new FileNotFoundException("Invalid Path");
    	}
    	
    	//start lock functionality
    	//I wanna go through each node down the path and lock them
    	Node cur = dirTree;
    	Path copy = new Path(path.toString());
    	Iterator<String> it = path.iterator();
   		while(it.hasNext()){
       		String next = it.next();
       		int id;
       		Lock curlock = cur.lock;
           	id = curlock.enq(false);
       		while(cur.hasWrite(id)){
       			synchronized(this){
       				try{
               			wait();
               		} catch (InterruptedException e){
               			continue;
                	}
       			}
       			
       		}
       		boolean flag = false;
       		Branch cur1 = ((Branch)cur);
       		for(int i = 0; i < cur1.list.size(); i++){
       			if(cur1.list.get(i).name.equals(next)){
       				cur = cur1.list.get(i);
       				flag = true;
       			}
       		}
       		if(!flag){
       			throw new FileNotFoundException("Bad Path.");
       		}
       	}
   		//Now I'm at the file, gotta add my lock to its list
   		int id;
   		
   		Lock curlock = cur.lock;
       	id = curlock.enq(exclusive);
    	while(cur.hasWrite(id) && !exclusive){
   			synchronized(this){
   				try{
   	       			wait();
   	       		} catch (InterruptedException e){
   	       			continue;
   	        	}
   			}
    		
    	}
    	while(!cur.isTop(id) && exclusive){
   			synchronized(this){
   				try{
   	       			wait();
   	       		} catch (InterruptedException e){
   	    			continue;
   	       		}
   			}
    		
    	}
    	
    	
    	if(!this.isDirectory(copy)){
    		// Replication code
    		double alpha = 0.05;
    		
    		Leaf file = (Leaf)cur;
    		int nreplicas = Integer.min(((int)(alpha*file.numRequests)), this.commands.size());
    		
    		if(exclusive){
    			for(int i = 0; i < file.replicas.size(); i++){
    				file.replicas.get(i).delete(copy);
    			}
    			file.replicas.clear();
    			file.numRequests = 0;
    		} else {
    			file.numRequests+=1;
    			if(nreplicas <= file.replicas.size()){
        			return;
        		}
    			for(int i = 0; i < this.commands.size(); i++){
    				if(!commands.get(i).equals(file.c) && !file.replicas.contains(commands.get(i))){
    					
    					try {
							commands.get(i).copy(copy, file.s);
						} catch (IOException e) {
							// Never happens
							continue;
						}
    					file.replicas.add(commands.get(i));
    					return;
    				}
    			}
    		}
    		
    	}
    	
    }
    
    @Override
    public synchronized void unlock(Path path, boolean exclusive) throws RMIException {
    	try{
    		this.isDirectory(path);
    	} catch (FileNotFoundException e){
    		throw new IllegalArgumentException("Invalid Path");
    	}
    	Node cur = dirTree;
    	Iterator<String> it = path.iterator();
   		while(it.hasNext()){
       		String next = it.next();
       		Lock curlock = cur.lock;
       		if(curlock.waiters.size()>0){
       			try {
       				curlock.deq();
       				synchronized(this){
       					notifyAll();
       				}
       				
       			} catch (IOException e){
       				//Never happens(Guaranteed by if clause)
       			}
       		} 
       		boolean flag = false;
       		Branch cur1 = ((Branch)cur);
       		for(int i = 0; i < cur1.list.size(); i++){
       			if(cur1.list.get(i).name.equals(next)){
       				cur = cur1.list.get(i);
       				flag = true;
       			}
       		}
       		if(!flag){
       			throw new IllegalArgumentException("Bad Path.");
       		}
       	}
   		if(cur.lock.waiters.size()>0){
   			try {
   				cur.lock.deq();
   				synchronized(this){
   					notifyAll();
   				}
   			} catch (IOException e){
   				//Never happens(Guaranteed by if clause)
   			}
   		}
   		
    }
    
    @Override
    public boolean isDirectory(Path path) throws FileNotFoundException
    {
    	// Creates copy of path to iterate over
    	Path copy = new Path(path.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	
    	/*
    	 * Goes down directory tree, and checks if the current iteration is
    	 * a file or a directory.
    	 */
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				Node cur = pointer.list.get(i);
    				if(cur.getClass().equals(Leaf.class)){
    					return false;
    				} else {
    					if(!iter.hasNext()){
    						return true;
    					} else {
    						pointer = (Branch)cur;
    					}
    				}
    				
    			}
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
    	//If I get here then root was input
        return true;
    }

    @Override
    public String[] list(Path directory) throws FileNotFoundException
    {
    	// Creates copy of path to iterate over
        String[] li;
        Path copy = new Path(directory.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	
    	/*
    	 * Goes down to the directory to list.
    	 */
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				Node cur = (Node)pointer.list.get(i);
    				if(cur.getClass().equals(Leaf.class)) {
    					throw new FileNotFoundException("This is a file, not directory");
    				} else {
    					pointer = (Branch)cur;
    				}
    			}
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
    	// Lists the directory's children:
    	li = new String[pointer.list.size()];
    	for(int i = 0; i < pointer.list.size(); i++){
    		li[i] = pointer.list.get(i).name;
    	}
    	return li;
    }

    @Override
    public boolean createFile(Path file)
        throws RMIException, FileNotFoundException
    {
    	if(stubs.size()==0){
    		throw new IllegalStateException("No Storage stubs connected...");
    	}
    	
    	// Creates copy of path to iterate over
    	int r = (new Random()).nextInt(commands.size());
    	Path copy = new Path(file.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	
    	/*
    	 * Goes down the tree to get to the required directory of the 
    	 * file to be created in.
    	 */
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				Node cur = pointer.list.get(i);
    				if(cur.getClass().equals(Leaf.class)){
    					if(iter.hasNext()){
    						throw new FileNotFoundException("Invalid directory: child of file!?");
    					}
    					return false;
    				} else {
    					if(iter.hasNext()){
    						pointer = (Branch)cur;
    					} else {
    						return false;
    					}
    				}
    				
    			}
    		}
    		if(!flag && !iter.hasNext()){
    			// Reaches here if directory of file is found.
    			Leaf newFile = new Leaf(next, commands.get(r), stubs.get(r));
    			//adds to dirTree and asks required storageserver to create the file.
    			pointer.list.add(newFile);
    			commands.get(r).create(file);
    			return true;
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
    	// Code cannot reach here
        return false;
    }

    @Override
    public boolean createDirectory(Path directory) throws FileNotFoundException
    {
    	// Creates copy of path to iterate over
    	Path copy = new Path(directory.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	
    	// Goes to directory where the creation must happen
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				Node cur = pointer.list.get(i);
    				if(cur.getClass().equals(Leaf.class)){
    					if(iter.hasNext()){
    						throw new FileNotFoundException("Invalid directory: child of file!?");
    					}
    					return false;
    				} else {
    					pointer = (Branch)cur;
    				}
    				
    			}
    		}
    		if(!flag && !iter.hasNext()){
    			// We are in the parent of the creation spot.
    			Branch newdir = new Branch(next, new ArrayList<Node>());
    			pointer.list.add(newdir);
    			return true;
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
        return false;
    }
    
    public Node getNode(Path p) throws FileNotFoundException{
        Path copy = new Path(p.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	
    	/*
    	 * Goes down to the directory to list.
    	 */
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				Node cur = (Node)pointer.list.get(i);
    				if(this.isDirectory(p)){
    					pointer = (Branch)cur;
    				} else {
    					return cur;
    				}
    				
    			}
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
    	return (Node)pointer;
    }

    @Override
    public boolean delete(Path path) throws FileNotFoundException
    {
    	
    	if(this.isDirectory(path)){
    		Branch dir = (Branch)getNode(path);
    		for(int i = 0; i < dir.list.size(); i++){
    			if(this.isDirectory(new Path(path,dir.list.get(i).name))){
    				delete(new Path(path,dir.list.get(i).name));
    			} else {
    				Leaf file = (Leaf)dir.list.get(i);
    	    		for(int j = 0; j < file.replicas.size(); j++){
    	    			try{
    	    				file.replicas.get(j).delete(new Path(path.toString()));
    	    			} catch (RMIException e){
    	    				// Not happening
    	    				return false;
    	    			}
    	    		}
    	    		try{
    					file.c.delete(new Path(path.toString()));
    				} catch (RMIException e){
    					// Not happening
    					return false;
    				}
    	    		
    	    		Branch parent = (Branch)getNode(path.parent());
    	    		parent.list.remove(file);
    				
    			}
    		}
    	} else {
    		Leaf file = (Leaf)getNode(path);
    		for(int i = 0; i < file.replicas.size(); i++){
    			try{
    				file.replicas.get(i).delete(new Path(path.toString()));
    			} catch (RMIException e){
    				// Not happening
    				return false;
    			}
    		}
    		try{
				file.c.delete(new Path(path.toString()));
			} catch (RMIException e){
				// Not happening
				return false;
			}
    		
    		Branch parent = (Branch)getNode(path.parent());
    		parent.list.remove(file);
    	}
        return true;
    }
    
    public void deldirectory(Branch b, Path p){
    	Path parent = p.parent();
    	Iterator<String> it = parent.iterator();
    	Branch c = dirTree;
    	while(it.hasNext()){
    		String next = it.next();
    		for(int i = 0; i < c.list.size(); i++){
    			if(c.list.get(i).name.equals(next)){
    				c = ((Branch)c.list.get(i));
    			}
    		}
    	}
    	c.list.remove(b);
    	if(c.list.isEmpty()){
    		deldirectory(c,p.parent());
    	}
    }

    @Override
    public Storage getStorage(Path file) throws FileNotFoundException
    {
    	// Creates copy of path to iterate over
    	Path copy = new Path(file.toString());
    	Iterator<String> iter = copy.iterator();
    	boolean flag = false;
    	Branch pointer = dirTree;
    	Node cur;
    	
    	// Goes to the required file in the dirTree and gets its stub.
    	while(iter.hasNext()){
    		String next = iter.next();
    		for(int i = 0; i < pointer.list.size();i++){
    			if(next.equals(pointer.list.get(i).name)){
    				flag = true;
    				cur = pointer.list.get(i);
    				if(cur.getClass().equals(new Leaf("test").getClass())){
    					return ((Leaf)(cur)).s;
    				} else {
    					pointer = (Branch)cur;
    				}
    			}
    		}
    		if(!flag){
    			throw new FileNotFoundException("Path not valid in this file system");
    		}
    		flag = false;
    	}
    	//Code cannot reach here
        throw new FileNotFoundException("A miracle happened!");
    }

    // The method register is documented in Registration.java.
    @Override
    public Path[] register(Storage client_stub, Command command_stub,
                           Path[] files)
    {
    	// Checks arguments
    	if(client_stub==null || command_stub==null || files == null){
    		throw new NullPointerException("Null Args to register");
    	}
    	for(int i = 0; i<stubs.size(); i++){
    		if(client_stub.equals(stubs.get(i))){
    			throw new IllegalStateException("Server Already registered!");
    		}
    	}
        
    	
    	/*
    	 *  Adds all the paths to the directory tree and documents the
    	 *  command, storage stub of each file, while simultaneously
    	 *  building up the list of duplicates. 
    	 */
    	ArrayList<Path> duplicates = new ArrayList<Path>();
    	commands.add(command_stub);
    	stubs.add(client_stub);
    	for(int j = 0; j < files.length; j++){
    		Path copy = new Path(files[j].toString());
        	Iterator<String> iter = copy.iterator();
        	boolean flag = false;
        	Branch pointer = dirTree;
        	while(iter.hasNext()){
        		String next = iter.next();
        		for(int i = 0; i < pointer.list.size();i++){
        			if(next.equals(pointer.list.get(i).name)){
        				flag = true;
        				if(!iter.hasNext()){
        					duplicates.add(files[j]);
        				} else {
        					pointer = (Branch)pointer.list.get(i);
        				}
        			}
        		}
        		if(!flag){
        			if(iter.hasNext()){
        				Branch dir = new Branch(next, new ArrayList<Node>());
        				pointer.list.add(dir);
        				pointer = dir;
        			} else{
        				Leaf file = new Leaf(next, command_stub, client_stub);
        				pointer.list.add(file);
        			}
        		}
        		flag = false;
        	}
    	}
    	Path[] duplicatesFin = new Path[duplicates.size()];
    	duplicates.toArray(duplicatesFin);
    	return duplicatesFin;
    }
}
