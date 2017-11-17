package Uebung3;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ChatServerImpl extends UnicastRemoteObject implements ChatServer {

	private Map<ChatClient, String> clientNames;
	
	protected ChatServerImpl() throws RemoteException {
		super();
		clientNames = new HashMap<ChatClient, String>();
	}

	public boolean register(ChatClient c, String name) throws RemoteException {
		if (clientNames.values().contains(name)){
			return false;
		}
		for(ChatClient client : clientNames.keySet()) {
			sendToClient(client, "neuer Client: " + name);
		}
		sendToClient(c, "User im Chat: " + getUsers().toString());
		
		clientNames.put(c, name);
		return true;
	}

	public List<String> getUsers() throws RemoteException {
		return new ArrayList<String>(clientNames.values());
	}

	public void logout(ChatClient c) throws RemoteException {
		String name = clientNames.get(c);
		clientNames.remove(c);
		for(ChatClient client : clientNames.keySet()) {
			sendToClient(client, "Benutzer " + name + " hat den Chat verlassen!");
		}

	}

	public void send(String msg) throws RemoteException {
		for(ChatClient client : clientNames.keySet()){
			sendToClient(client, msg);
		}
	}
	
	private void sendToClient(ChatClient c, String msg) throws RemoteException{
		try {
			c.send(msg);
		} catch (RemoteException e){
			logout(c);
		}
	}

    /** Create a new registry or connect to the existing one */
    private static Registry getOrCreateRegistry() {
        try {
            return LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
        } catch (RemoteException e) {
            try {
                return LocateRegistry.getRegistry();
            } catch (RemoteException ee) {
                return null;
            }
        }
    }

	
	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {
		
		Registry reg = getOrCreateRegistry();
		ChatServerImpl server = new ChatServerImpl();
		reg.rebind(ChatServer.RMI_NAME, server);
		
	}

}
