package storage;

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;

import common.*;
import rmi.*;
import naming.*;

/** Storage server.

    <p>
    Storage servers respond to client file access requests. The files accessible
    through a storage server are those accessible under a given directory of the
    local filesystem.
 */
public class StorageServer implements Storage, Command
{
	public File root;
	public ArrayList<Path> filepaths;
	public Skeleton<Storage> storageServer;
	public Skeleton<Command> commandServer;
	public Command commandStub;
	public Storage storageStub;
    /** Creates a storage server, given a directory on the local filesystem.

        @param root Directory on the local filesystem. The contents of this
                    directory will be accessible through the storage server.
        @throws NullPointerException If <code>root</code> is <code>null</code>.
    */
    public StorageServer(File root)
    {
    	if(root == null){
    		throw new NullPointerException("Root is null");
    	}
    	try{
    		filepaths = new ArrayList<Path>();
    		Path[] a = Path.list(root);
    		for(int i = 0; i < a.length; i++){
    			filepaths.add(a[i]);
    		}
    		this.root = root;
    	} catch (FileNotFoundException e){
    		filepaths = null;
    		e.printStackTrace();
    	}
    	// Initialize new command and storage skeletons
    	storageServer = new Skeleton<Storage>(Storage.class, this);
    	commandServer = new Skeleton<Command>(Command.class, this);
    }
    
    /** Creates a storage server, given a directory on the local filesystem.

    @param root Directory on the local filesystem. The contents of this
                directory will be accessible through the storage server.
    @param storage Port on which the storage server must be initialized.
    @param command Port on which the command server must be initialized
    @throws NullPointerException If <code>root</code> is <code>null</code>.
     */
    public StorageServer(File root, int storage, int command)
    {
    	if(root == null){
    		throw new NullPointerException("Root is null");
    	}
    	try{
    		filepaths = new ArrayList<Path>();
    		Path[] a = Path.list(root);
    		for(int i = 0; i < a.length; i++){
    			filepaths.add(a[i]);
    		}
    		this.root = root;
    	} catch (FileNotFoundException e){
    		filepaths = null;
    		e.printStackTrace();
    	}
    	InetSocketAddress st = new InetSocketAddress(storage);
    	InetSocketAddress com = new InetSocketAddress(command);
    	// Initialize new command and storage skeletons
    	storageServer = new Skeleton<Storage>(Storage.class, this, st);
    	commandServer = new Skeleton<Command>(Command.class, this, com);
}
    

    /** Starts the storage server and registers it with the given naming
        server.

        @param hostname The externally-routable hostname of the local host on
                        which the storage server is running. This is used to
                        ensure that the stub which is provided to the naming
                        server by the <code>start</code> method carries the
                        externally visible hostname or address of this storage
                        server.
        @param naming_server Remote interface for the naming server with which
                             the storage server is to register.
        @throws UnknownHostException If a stub cannot be created for the storage
                                     server because a valid address has not been
                                     assigned.
        @throws FileNotFoundException If the directory with which the server was
                                      created does not exist or is in fact a
                                      file.
        @throws RMIException If the storage server cannot be started, or if it
                             cannot be registered.
     */
    public synchronized void start(String hostname, Registration naming_server)
        throws RMIException, UnknownHostException, FileNotFoundException
    {
    	if(filepaths==null){
    		throw new FileNotFoundException("Bad directory as base for server");
    	}
    	
    	
    	try {
    		// Start skeletons
    		storageServer.start();
    		commandServer.start();
    	} catch (Exception e) {
    		throw new RMIException("Server cannot be started.");
    	}
    	try{
    		// Create corresponding stubs
    		storageStub = Stub.create(Storage.class, storageServer, hostname);
    		commandStub = Stub.create(Command.class, commandServer, hostname);
    	} catch (Exception e) {
    		throw new UnknownHostException("Valid address not assigned by skeleton");
    	} 
    	try{
    		// Remotely register with the naming server
    		Path[] a = new Path[filepaths.size()];
    		filepaths.toArray(a);
    		Path[] duplicates = naming_server.register(storageStub, commandStub, a);
    		
    		//removes duplicates
    		for(int i = 0; i < duplicates.length; i++){
    			Path cur = new Path(duplicates[i].toString());
    			Iterator<String> it = cur.iterator();
    			File pointer = root;
    			while(it.hasNext()){
    				String next = it.next();
    				File[] children = pointer.listFiles();
    				for(int j = 0; j < children.length; j++){
    					if((children[j].getName()).equals(next)){
    						pointer = children[j];
    					}
    				}
    			}
    			pointer.delete();
    			for(int j = 0; j < filepaths.size(); j++){
    				if(filepaths.get(j).equals(duplicates[i])){
    					filepaths.remove(j);
    				}
    			}	
    		}
    		
    	} catch (Exception e){
    		throw new RMIException("Server cant be started, or registration fails.");
    	}
    	
    	// prunes empty directories
    	this.prune(root);
    	
    }
    
    /*
     * Helper that prunes all empty directories in a local file system.
     */
    public void prune(File x){
    	if(x==null){
    		return;
    	}
    	File[] children = x.listFiles();
    	if(children==null){
    		return;
    	}
    	for(int i = 0; i < children.length; i++){
    		prune(children[i]);
    	}
    	File[] curchildren = x.listFiles();
    	if(curchildren.length==0){
    		x.delete();
    	}
    }

    /** Stops the storage server.

        <p>
        The server should not be restarted.
     */
    public void stop()
    {
    	storageServer.stop();
    	commandServer.stop();
    	stopped(null);
        
    }

    /** Called when the storage server has shut down.

        @param cause The cause for the shutdown, if any, or <code>null</code> if
                     the server was shut down by the user's request.
     */
    protected void stopped(Throwable cause)
    {
    }

    // The following methods are documented in Storage.java.
    @Override
    public synchronized long size(Path file) throws FileNotFoundException
    {
        Path x = new Path(file.toString());
        Iterator<String> it = x.iterator();
        File dir = root;
        String next;
        boolean flag = false;
        while(it.hasNext()){
        	next = it.next();
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		
        		if(next.equals(children[i].getName())){
        			dir = children[i];
        			flag = true;
        		}
        	}
        	if(!flag){
        		throw new FileNotFoundException("File not found");
        	}
        	flag = false;
        }
        if(dir == null || dir.isDirectory()){
        	throw new FileNotFoundException("Not valid file.");
        }
        return dir.length();
    }

    @Override
    public synchronized byte[] read(Path file, long offset, int length)
        throws FileNotFoundException, IOException
    {
    	
    	Path x = new Path(file.toString());
        Iterator<String> it = x.iterator();
        File dir = root;
        String next;
        boolean flag = false;
        while(it.hasNext()){
        	next = it.next();
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		if(next.equals(children[i].getName())){
        			dir = children[i];
        			flag = true;
        		}
        	}
        	if(!flag){
        		throw new FileNotFoundException("Bad path.");
        	}
        	flag = false;
        }
        if(dir==null || dir.isDirectory()){
        	throw new FileNotFoundException("Bad path: leads to directory.");
        }
        if(dir.length()==0){
        	byte[] a = new byte[0];
        	return a;
        }
        if(offset<0 || offset>=dir.length()){
        	throw new IndexOutOfBoundsException("Offset/length invalid.");
        }
        if(length-(int)offset>dir.length() || length<0){
        	throw new IndexOutOfBoundsException("Offset/length invalid.");
        }
        try {
        	
			InputStream in = new FileInputStream(dir);
			byte[] out = new byte[length-(int)offset];
			in.read(out, ((int)offset), length);
			in.close();
			return out;
			
			
		} catch (Exception e) {
			e.printStackTrace();
			throw new IOException("Read failed.");
		}
    }

    @Override
    public synchronized void write(Path file, long offset, byte[] data)
        throws FileNotFoundException, IOException
    {
    	if(offset<0){
    		throw new IndexOutOfBoundsException("Bad offset");
    	}
    	if(data==null){
    		throw new NullPointerException("Data stream is null");
    	}
    	Path x = new Path(file.toString());
        Iterator<String> it = x.iterator();
        File dir = root;
        String next;
        boolean flag = false;
        while(it.hasNext()){
        	next = it.next();
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		if(next.equals(children[i].getName())){
        			dir = children[i];
        			flag = true;
        		}
        	}
        	if(!flag){
        		throw new FileNotFoundException("Bad path.");
        	}
        	flag = false;
        }
        if(dir==null || dir.isDirectory()){
        	throw new FileNotFoundException("Bad path: leads to directory.");
        }
        
        try {
        	InputStream in = new FileInputStream(dir);
			FileOutputStream  out = new FileOutputStream(dir);
			if((int)offset+data.length>dir.length()){
				byte[] c = new byte[(int)offset+data.length];
				in.read(c, 0, (int)offset);
				int i;
				int j;
				for(i = (int)offset, j = 0; i < c.length; i++, j++){
					c[i] = data[j];
				}
				out.write(c);
			} else {
				out.write(data, (int)offset, data.length);
			}
			
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
			throw new IOException("Failed to write.");
		}
        
    }

    // The following methods are documented in Command.java.
    @Override
    public synchronized boolean create(Path file)
    {
    	Path x = new Path(file.toString());
        Iterator<String> it = x.iterator();
        File dir = root;
        String next = "/";
        boolean flag = false;
        while(it.hasNext()){
        	
        	next = it.next();
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		if(next.equals(children[i].getName())){
        			dir = children[i];
        			flag = true;
        		}
        	}
        	if(!flag && !it.hasNext()) {
        		try{
        			File newfile= new File(dir, next);
        			boolean success = newfile.createNewFile();
        			return success;
        		} catch (Exception e) {
        			return false;
        		}
        		
        	}
        	if(!flag){
        		dir = new File(dir, next);
        		dir.mkdir();
        		
        	}
        	flag = false;
        }
        
        return false;
    }

    @Override
    public synchronized boolean delete(Path path)
    {
    	if(path.isRoot()){
    		return false;
    	}
    	Path x = new Path(path.toString());
        Iterator<String> it = x.iterator();
        File dir = root;
        String next;
        boolean flag = false;
        while(it.hasNext()){
        	next = it.next();
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		if(next.equals(children[i].getName())){
        			dir = children[i];
        			flag = true;
        		}
        	}
        	if(!flag){
        		return false;
        	}
        	flag = false;
        }
        if(dir.isDirectory()){
        	File[] children = dir.listFiles();
        	for(int i = 0; i < children.length; i++){
        		Path pathi = new Path(path, children[i].getName());
        		this.delete(pathi);
        	}
        }
        return dir.delete();
    }
    
    @Override
    public boolean copy(Path file, Storage server)
            throws RMIException, FileNotFoundException, IOException{
    	if(file==null){
    		throw new NullPointerException();
    	}
    	
    	int size = (int)server.size(file);
    	
    	
    	File fileobj=file.toFile(root);
    	if(fileobj.exists()){
    		this.delete(file);
    	}
    	int offset = 0;
    	this.create(file);
    	while(size>1000000){
    		byte[] filecontent = server.read(file, offset, 1000000);        	
        	this.write(file, offset, filecontent);
        	offset = offset + 1000000;
        	size = size-1000000;
    	}
    	byte[] filecontent = server.read(file, offset, size);        	
    	this.write(file, offset, filecontent);
    	
    	return true;
        
    }
}
