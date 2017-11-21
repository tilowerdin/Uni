package Uebung4.Aufgabe1;

import java.rmi.AccessException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ChatServer extends UnicastRemoteObject implements IChatServer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Map<IChatClient, String> clients;

	public ChatServer() throws RemoteException {
		super();
		clients = new HashMap<>();
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Registry reg;
		try {
			reg = LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
		} catch (RemoteException e) {
			try {
				reg = LocateRegistry.getRegistry();
			} catch (RemoteException e1) {
				System.out.println("Unable to create a registry... finishing");
				return;
			}
		}
		ChatServer server;
		try {
			server = new ChatServer();
			reg.rebind(IChatServer.name, server);
		} catch (RemoteException e) {
			System.out.println("Unable to create Server... finishing");
		}

	}

	@Override
	public Set<String> login(IChatClient obj, String name) {
		Set<String> result = new HashSet<>();
		result.addAll(clients.values());

		broadcast(name + " joined.", obj);

		clients.put(obj, name);
		return result;
	}

	@Override
	public void logout(IChatClient obj) {
		String name = clients.remove(obj);
		broadcast(name + " left.", obj);
	}

	@Override
	public void write(IChatClient obj, String msg) {
		broadcast(clients.get(obj) + ": " + msg, obj);
	}

	private void broadcast(String msg, IChatClient sender) {
		System.out.println("broadcast message: " + msg);
		Set<IChatClient> disconnected = new HashSet<>();
		for (IChatClient client : clients.keySet()) {
			if (!client.equals(sender)) {
				try {
					client.getMessage(msg);
					System.out.println("sent messsage to: "
							+ clients.get(client));
				} catch (RemoteException e) {
					System.out.println(clients.get(client) + " disconnected");
					disconnected.add(client);
				}
			}
		}

		Set<String> names = new HashSet<>();
		for (IChatClient client : disconnected) {
			names.add(clients.remove(client));
		}

		for (String name : names) {
			broadcast(name + " left.", null);
			System.out.println(name + " left.");
		}
	}

}
