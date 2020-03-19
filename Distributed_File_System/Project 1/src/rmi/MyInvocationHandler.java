package rmi;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetSocketAddress;
import java.net.Socket;

public class MyInvocationHandler<T> implements InvocationHandler,Serializable {
	/**
	 * Data members:
	 * 
	 * InetSocketAddress newAddress:
	 * Holds the address of the skeleton to send requests to.
	 * 
	 */
	public InetSocketAddress newAddress;
	
	
	/*
	 * Constructor:
	 * Gets an address to connect to and makes an invocationhandler accordingly
	 */
	MyInvocationHandler(InetSocketAddress add){
		newAddress = add;
	}
	
	/*
	 * invoke executes the method called by a user.
	 * 
	 * It either executes it directly if it is local,
	 * or forwards it to the Skeleton and has it executed.
	 * It returns the result, or throws it if it is an exception.
	 * 
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	public Object invoke (Object proxy, Method method, Object[] args) throws Throwable{
		
		// Executing local methods:
		if (method.getName()=="equals"){
			Proxy y = (Proxy)args[0]; //this is the second proxy
			try{
				InetSocketAddress add1;
				InetSocketAddress add2;
				MyInvocationHandler<T> sk2 = (MyInvocationHandler<T>)Proxy.getInvocationHandler(y);
				add2 = sk2.newAddress;
				add1 = this.newAddress;
				return add1.equals(add2);
			} catch (Exception e){
				return false;
			}
		}
		else if (method.getName()=="hashCode"){
			return this.newAddress.getPort();
			
		}
		else if (method.getName()=="toString"){
			return ("Port: "+Integer.toString(this.newAddress.getPort()));
			
		}
		// If the method is not local, forward to skeleton
		else{
			Object result;
			try {
				Socket clientStub = new Socket();
				clientStub.connect(newAddress);
				ObjectOutputStream out = new ObjectOutputStream(clientStub.getOutputStream());
				ObjectInputStream in = new ObjectInputStream(clientStub.getInputStream());
				out.writeObject(method.getName());
				out.writeObject(args);
				out.writeObject(method.getParameterTypes());
				result = in.readObject();
				
				// Throws if exception, else returns
				if(result != null && (Exception.class).isInstance(result)){
					in.close();
					out.close();
					clientStub.close();
				} else{
					in.close();
					out.close();
					clientStub.close();
					return result;
				}
				
			} catch (Exception e) {
				throw new RMIException("Problem with skeleton: no data stream.");
				
			}
			throw (Throwable)result;
		}
	}
}
