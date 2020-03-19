package rmi;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.*;



/** RMI skeleton

    <p>
    A skeleton encapsulates a multithreaded TCP server. The server's clients are
    intended to be RMI stubs created using the <code>Stub</code> class.

    <p>
    The skeleton class is parametrized by a type variable. This type variable
    should be instantiated with an interface. The skeleton will accept from the
    stub requests for calls to the methods of this interface. It will then
    forward those requests to an object. The object is specified when the
    skeleton is constructed, and must implement the remote interface. Each
    method in the interface should be marked as throwing
    <code>RMIException</code>, in addition to any other exceptions that the user
    desires.

    <p>
    Exceptions may occur at the top level in the listening and service threads.
    The skeleton's response to these exceptions can be customized by deriving
    a class from <code>Skeleton</code> and overriding <code>listen_error</code>
    or <code>service_error</code>.
*/
public class Skeleton<T>
{
	/* Data Members of Skeleton:
	 * Class<T> c:
	 * This is an object that represents the class of the interface of the skeleton
	 * 
	 * T server:
	 * This is an object that implements the interface c
	 * 
	 * InetSocketAddress serverAddress:
	 * The address of the server, in the InetSocketAddress form.
	 * 
	 * ServerSocket listenSocket:
	 * This is the socket thats open to connection requests and deals with them.
	 * 
	 * boolean running:
	 * true if the skeleton is still listening.
	 * 
	 * Thread listen: 
	 * represents the main listening thread
	 * 
	 * int port:
	 * The port on which the skeleton is running.
	 * 
	 */
	Class<T> c;
	T server;
	InetSocketAddress serverAddress;
	ServerSocket listenSocket;
	boolean running;
	Thread listen;
	int port=0;
	
	/*
	 * End of data member declarations.
	 */
	
	/*
	 * Beginning of constructors:
	 */
	
    /** Creates a <code>Skeleton</code> with no initial server address. The
        address will be determined by the system when <code>start</code> is
        called. Equivalent to using <code>Skeleton(null)</code>.

        <p>
        This constructor is for skeletons that will not be used for
        bootstrapping RMI - those that therefore do not require a well-known
        port.

        @param c An object representing the class of the interface for which the
                 skeleton server is to handle method call requests.
        @param server An object implementing said interface. Requests for method
                      calls are forwarded by the skeleton to this object.
        @throws Error If <code>c</code> does not represent a remote interface -
                      an interface whose methods are all marked as throwing
                      <code>RMIException</code>.
        @throws NullPointerException If either of <code>c</code> or
                                     <code>server</code> is <code>null</code>.
     */
    public Skeleton(Class<T> c, T server)
    {
    	if (c==null || server==null){
    		throw new NullPointerException();
    	}
    	boolean flag = false;
    	Method[] x = c.getMethods();
    	for(int i=0; i < x.length; i++){
    		Class<?>[] y = x[i].getExceptionTypes();
    		for(int j=0; j < y.length; j++){
    			if(y[j].getName()=="rmi.RMIException"){
    				flag = true;
    			}
    		}
    		if(!flag){
    			throw new Error("Wrong class definition");
    		}
    		else{
    			flag = false;
    			continue;
    		}
    	}
    	this.c = c;
    	this.server = server;
    	this.serverAddress = null;
    	running = false;
    }

    /** Creates a <code>Skeleton</code> with the given initial server address.

        <p>
        This constructor should be used when the port number is significant.

        @param c An object representing the class of the interface for which the
                 skeleton server is to handle method call requests.
        @param server An object implementing said interface. Requests for method
                      calls are forwarded by the skeleton to this object.
        @param address The address at which the skeleton is to run. If
                       <code>null</code>, the address will be chosen by the
                       system when <code>start</code> is called.
        @throws Error If <code>c</code> does not represent a remote interface -
                      an interface whose methods are all marked as throwing
                      <code>RMIException</code>.
        @throws NullPointerException If either of <code>c</code> or
                                     <code>server</code> is <code>null</code>.
     */
    public Skeleton(Class<T> c, T server, InetSocketAddress address)
    {
    	if (c==null || server==null){
    		throw new NullPointerException();
    	}
    	boolean flag = false;
    	Method[] x = c.getMethods();
    	for(int i=0; i < x.length; i++){
    		Class<?>[] y = x[i].getExceptionTypes();
    		for(int j=0; j < y.length; j++){
    			if(y[j].getName()=="rmi.RMIException"){
    				flag = true;
    			}
    		}
    		if(!flag){
    			throw new Error("Wrong class definition");
    		}
    		else{
    			flag = false;
    			continue;
    		}
    	}
    	this.c = c;
    	this.server = server;
    	this.serverAddress = address;
    	running = false;
    }
    
    /*
     * End of Skeleton constructors.
     */
    
    /*
     * Beginning of Skeleton methods.
     */

    /** Called when the listening thread exits.

        <p>
        The listening thread may exit due to a top-level exception, or due to a
        call to <code>stop</code>.

        <p>
        When this method is called, the calling thread owns the lock on the
        <code>Skeleton</code> object. Care must be taken to avoid deadlocks when
        calling <code>start</code> or <code>stop</code> from different threads
        during this call.

        <p>
        The default implementation does nothing.

        @param cause The exception that stopped the skeleton, or
                     <code>null</code> if the skeleton stopped normally.
     */
    protected void stopped(Throwable cause)
    {
    	
    }

    /** Called when an exception occurs at the top level in the listening
        thread.

        <p>
        The intent of this method is to allow the user to report exceptions in
        the listening thread to another thread, by a mechanism of the user's
        choosing. The user may also ignore the exceptions. The default
        implementation simply stops the server. The user should not use this
        method to stop the skeleton. The exception will again be provided as the
        argument to <code>stopped</code>, which will be called later.

        @param exception The exception that occurred.
        @return <code>true</code> if the server is to resume accepting
                connections, <code>false</code> if the server is to shut down.
     */
    protected boolean listen_error(Exception exception)
    {
        return false;
    }

    /** Called when an exception occurs at the top level in a service thread.

        <p>
        The default implementation does nothing.

        @param exception The exception that occurred.
     */
    protected void service_error(RMIException exception)
    {
    }

    /** Starts the skeleton server.

        <p>
        A thread is created to listen for connection requests, and the method
        returns immediately. Additional threads are created when connections are
        accepted. The network address used for the server is determined by which
        constructor was used to create the <code>Skeleton</code> object.

        @throws RMIException When the listening socket cannot be created or
                             bound, when the listening thread cannot be created,
                             or when the server has already been started and has
                             not since stopped.
     */
    public synchronized void start() throws RMIException
    {
    	if(running == true){
    		throw new RMIException("Server already started...");
    	}
    	// this keeps track of whether the server is listening or not
    	running = true;
    	
    	//set serverAddress if uninitialized
    	try{
    		if(serverAddress == null){
    			listenSocket = new ServerSocket(0);
    			port=listenSocket.getLocalPort();
    			serverAddress = new InetSocketAddress(port);
    			
    		} else {
    			this.listenSocket = new ServerSocket();
    			port=serverAddress.getPort();
    			listenSocket.bind(serverAddress);
    		}
    		
    		
    		listenSocket.setReuseAddress(true);
    		Listener l = new Listener();
            this.listen = new Thread(l);
            listen.start();
    	} catch (Exception e){
    		e.printStackTrace();
    		throw new RMIException("Listener cannot be started");
    	}
        
    }//End of start
    
    /** Stops the skeleton server, if it is already running.
	
	    <p>
	    The listening thread terminates. Threads created to service connections
	    may continue running until their invocations of the <code>service</code>
	    method return. The server stops at some later time; the method
	    <code>stopped</code> is called at that point. The server may then be
	    restarted.
	 */
	public synchronized void stop()
	{
		running = false;
		try{
			listenSocket.close();
		} catch(NullPointerException e){
			//do nothing its annoying tbh
		} catch (Exception e){
			e.printStackTrace();
		}
		stopped(null);
		
	}//End of stop
	
	/*
	 * End of Skeleton methods
	 */
	
	/*
	 * Beginning of Thread logic
	 */
	
	/*
	 * This class represents the functionality of the main Listening Thread.
	 * 
	 * It listens for connect attempts and routs them each to a service thread.
	 * 
	 * It only stops executing when the Skeleton is stopped.
	 */
	public class Listener implements Runnable {
		/*
		 * The logic of this function involves reading the method name, its arguments, and their types, and finding 
		 * from the given interface c. We then invoke the method from the implementation, which is in server.
		 * We then write out the object we get as a result of this. 
		 */
		public void run() {
			Socket serviceSocket;
			try{
				while (running) {
					serviceSocket = listenSocket.accept();
					Service callx = new Service(serviceSocket);
					Thread t = new Thread(callx);
					t.start();
				}
	        } catch(Exception e){
	        	if(!running){
	        	} else {
	        		//do nothing
	        	}
	        }
		}
	}//end of Listener
    
    
    /*
	 * A class that can be run in a thread to service a client stub by calling
	 * the function for the client from the given 
	 * implementation (server).
	 */
	public class Service implements Runnable {
		Socket serviceSocket = null;
		ObjectInputStream in = null;
		ObjectOutputStream out = null;

		/*
		 * Sets the server, in and out attributes from the serviceSocket given
		 */
		public Service(Socket s) {
			try {
				this.serviceSocket = s;
				this.out = new ObjectOutputStream(serviceSocket.getOutputStream());
				this.in = new ObjectInputStream(serviceSocket.getInputStream());
				this.out.flush();
			} catch (Exception e) {
				if(!running){
	        	} else {
	        		//do nothing
	        	}
			}
		}
		

		/*
		 * The logic of this function involves reading the method name, its arguments, and their types, and finding 
		 * from the given interface c. We then invoke the method from the implementation, which is in server.
		 * We then write out the object we get as a result of this. 
		 */
		public void run() {
			try {
				String methodName = (String)this.in.readObject();
				Object[] args = (Object[])in.readObject();
				@SuppressWarnings("unchecked")
				Class<T>[] argTypes = (Class<T>[])in.readObject();
				Method m = c.getMethod(methodName, argTypes); //check if java.lang.reflect
				Object result;
				try{
					result = m.invoke(server, args);
				} catch(InvocationTargetException e){
					result = e.getTargetException();
				}
				out.writeObject(result);
				serviceSocket.close();
			} catch (Exception e) {
				if(!running){
	        	} else {
	        		//e.printStackTrace();
	        	}
			}
		}
	}//end of Service
	/*
	 * End of thread logic.
	 */
}//end of Skeleton
