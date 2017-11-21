package Uebung4.Aufgabe1;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

public interface IChatServer extends Remote {

	public static final String name = "chat_server";
	
	public Set<String> login(IChatClient obj, String name) throws RemoteException;
	
	public void logout(IChatClient obj) throws RemoteException;
	
	public void write(IChatClient obj, String msg) throws RemoteException;
	
}
