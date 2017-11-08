package Uebung3;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

/**
 * This is an interface for a simple RMI chat server.
 */
public interface ChatServer extends Remote {

    public final String RMI_NAME = "chatserver";

    /**
     * Register a client at the server
     * 
     * @param c
     *            the client to register
     * @exception RemoteException
     *                if an error occurs
     */
    public boolean register(ChatClient c, String name) throws RemoteException;

    /**
     * Retrieve the collection of users
     * 
     * @return the list of users currently logged in 
     * @throws RemoteException
     */
    public List<String> getUsers() throws RemoteException;

    /**
     * Remove a client from the server
     * 
     * @param c
     *            the client to remove
     * @exception RemoteException
     *                if an error occurs
     */
    public void logout(ChatClient c) throws RemoteException;

    /**
     * Send a message to all connected clients
     *
     * @param msg
     *            the message to send
     * @exception RemoteException
     *                if an error occurs
     */
    public void send(String msg) throws RemoteException;

}
