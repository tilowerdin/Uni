package Uebung4.Aufgabe1;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IChatClient extends Remote {

	public void getMessage(String msg) throws RemoteException;
	
}
