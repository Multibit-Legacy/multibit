package org.multibit.viewsystem.commandline.rxtx;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Vector;

/**
 * This is a very simple example showing the most basic use of
 * {@link net.Network} and {@link net.Network_iface}. Feel free to use,
 * overwrite, or just ignore code as you like.
 * 
 * As a default, a connection speed of 115200 baud is assumed. You can use a
 * different speed by giving it as an <b>int</b> as the first command line
 * argument or changing the default speed in the source code.
 * 
 * @author Raphael Blatter (raphael@blatter.sg)
 * @author Jim Burton
 */
public class Example implements Network_iface {

	// set the speed of the serial port
	public static int speed = 115200;
	private static Network network;

	public static void main(String[] args) {
		try {
			network = new Network(0, new Example(), 255);

			// reading the speed if
			if (args.length > 0) {
				try {
					speed = Integer.parseInt(args[0]);
				} catch (NumberFormatException e) {
					System.out.println("The speed must be an integer\n");
					System.exit(1);
				}
			}

			// initializing reader from command line
			int i, portToUse = 0;
			String input;
			BufferedReader in_stream = new BufferedReader(
					new InputStreamReader(System.in));

			// Get a list of the available serial ports
			Vector<String> ports = network.getPortList();

			// Choose the port to connect to
			System.out.println();
			if (ports.size() == 0) {
				System.out
						.println("No serial ports were found on your computer\n");
				System.exit(0);
			} else if (ports.size() == 1) {
				portToUse = 1;
				System.out.println("Using the following port:  "
						+ ports.elementAt(0));
			} else {
				System.out
						.println("The following serial ports have been detected:");
				for (i = 0; i < ports.size(); ++i) {
					System.out.println("    " + Integer.toString(i + 1) + ":  "
							+ ports.elementAt(i));
				}

				boolean validAnswer = false;
				while (!validAnswer) {
					System.out
							.println("Enter the id (1,2,...) of the connection to connect to: ");
					try {
						input = in_stream.readLine();
						portToUse = Integer.parseInt(input);
						if ((portToUse < 1) || (portToUse >= ports.size() + 1))
							System.out.println("Your input is not valid");
						else
							validAnswer = true;
					} catch (NumberFormatException ex) {
						System.out.println("Please enter a correct number");
					} catch (IOException e) {
						System.out.println("There was an input error\n");
						System.exit(1);
					}
				}
			}

			// Connect to the selected port
			if (network.connect(ports.elementAt(portToUse - 1), speed)) {
				System.out.println();
			} else {
				System.out.println("There was an error connecting\n");
				System.exit(1);
			}

			// reading in numbers (bytes) to be sent over the serial port
			System.out.println("Type 'q' to quit.");
			while (true) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
				}
				System.out
						.println("\nEnter a string to be sent ('q' to quit): ");
				try {
					input = in_stream.readLine();
					if (input.equals("q")) {
						System.out.println("Example terminated\n");
						network.disconnect();
						System.exit(0);
					}
					byte[] bytes = input.getBytes();
					for (int j = 0; j < bytes.length; j++) {
						int[] temp = { (new Integer(bytes[j])).intValue() };
						network.writeSerial(1, temp);
						System.out.println("Sent " + bytes[j]
								+ " over the serial port.");
					}
				} catch (IOException e) {
					System.out.println("There was an input error");
				}
			}
		} catch (UnsatisfiedLinkError ule) {
			System.out.println("rxtxSerial is not on the java.library.path = "
					+ System.getProperty("java.library.path"));
		}
	}

	/**
	 * Implementing {@link net.Network_iface#networkDisconnected(int)}, which is
	 * called when the connection has been closed. In this example, the program
	 * is ended.
	 * 
	 * @see net.Network_iface
	 */
	public void networkDisconnected(int id) {
		System.exit(0);
	}

	/**
	 * Implementing {@link net.Network_iface#parseInput(int, int, int[])} to
	 * handle messages received over the serial port. In this example, the
	 * received bytes are written to command line (0 to 254)
	 * 
	 * @see net.Network_iface
	 */
	public void parseInput(int id, int numBytes, int[] message) {
		System.out.print("received the following message: ");
		System.out.print(message[0]);
		for (int i = 1; i < numBytes; ++i) {
			System.out.print(", ");
			System.out.print((char) message[i]);
		}
		System.out.println();
	}

	/**
	 * Implementing {@link net.Network_iface#writeLog(int, String)}, which is
	 * used to write information concerning the connection. In this example, all
	 * the information is simply written out to command line.
	 * 
	 * @see net.Network_iface
	 */
	public void writeLog(int id, String text) {
		System.out.println("   log:  |" + text + "|");
	}
}
