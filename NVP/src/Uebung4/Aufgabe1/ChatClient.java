package Uebung4.Aufgabe1;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Scanner;

import com.sun.org.apache.xpath.internal.functions.WrongNumberArgsException;

public class ChatClient extends UnicastRemoteObject implements IChatClient {

	private IChatServer server;
	private String name;

	protected ChatClient(IChatServer server, String name) throws RemoteException {
		super();
		this.server = server;
		this.name = name;
	}

	private static final long serialVersionUID = 1L;

	@Override
	public void getMessage(String msg) throws RemoteException {
		System.out.println(msg);
	}

	public void run() {
		// trying to login to the server
		try {
			server.login(this, name);
			boolean connected = true;
			Scanner in = new Scanner(System.in);
			System.out.println("Welcome in our ChatRoom. Type ':q' to leave.");
			while (connected) {
				String msg = in.nextLine();
				switch (msg) {
				case ":q":
					server.logout(this);
					connected = false;
					break;
				default:
					server.write(this, msg);
				}
			}
			in.close();
		} catch (RemoteException e) {
			System.out.println("Connection to the server lost.");
		}
	}
	
	/**
	 * @param args
	 * @throws WrongNumberArgsException 
	 */
	public static void main(String[] args) throws WrongNumberArgsException {
		
		if(args.length < 3)
			throw new WrongNumberArgsException(
					"you need to pass three arguments to the program:\n" +
					"ChatClient <host> <chat_name> <username>"
					);
		
		try {
			Registry reg = LocateRegistry.getRegistry(args[0]);
			IChatServer server = (IChatServer) reg.lookup(args[1]);
			ChatClient client = new ChatClient(server, args[2]);
			client.run();
		} catch (RemoteException e) {
			System.out.println("Unable to find the hosts registry");
		} catch (NotBoundException e) {
			System.out.println("Unable to find the given chatname");
		}
		System.exit(0);
	}

}
