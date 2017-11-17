package Uebung3;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Scanner;

public class ChatClientImpl extends UnicastRemoteObject implements ChatClient {

	private ChatServer server;
	private String name;
	
	protected ChatClientImpl(ChatServer s, String name) throws RemoteException {
		super();
		server = s;
		this.name = name;
	}

	public void send(String msg) throws RemoteException {
		System.out.println(msg);
	}
    
    public void start() throws RemoteException{
    	server.register(this, name);
        System.out.println("Welcome to talk, type ':q' to quit.");
        try (Scanner in = new Scanner(System.in)) {
            String msg;
            boolean connected = true;
            while (connected) {
                msg = in.nextLine();
				if (msg.equals(":q")) {
					server.logout(this);
					connected = false;
				} else {
					server.send(name + ": " + msg);
				}
                
            }
        }

    }
	
	/**
	 * @param args
	 * @throws RemoteException 
	 * @throws NotBoundException 
	 */
	public static void main(String[] args) throws Exception {
		if(args.length < 1) {
			System.out.println("Name als Parameter übergeben");
			System.exit(1);
		}
		
		Registry reg = LocateRegistry.getRegistry("localhost");
		ChatServer server = (ChatServer) reg.lookup(ChatServer.RMI_NAME);
		ChatClientImpl client = new ChatClientImpl(server, args[0]);
		client.start();
		System.exit(0);
	}

}
