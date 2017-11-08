package Uebung3;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * This is an interface for a simple RMI chat client
 */
public interface ChatClient extends Remote {

    /**
     * Receive a message from the server and print it
     * 
     * @exception RemoteException
     *                if an error occurs
     */
    public void send(String msg) throws RemoteException;

}
