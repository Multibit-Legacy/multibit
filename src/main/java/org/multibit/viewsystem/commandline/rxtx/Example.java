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
 */
public class Example implements Network_iface {

	// set the speed of the serial port
	public static int speed = 115200;
	private static Network network;

	private static boolean resend_active = false;

	public static void main(String[] args) {
	    try {
		network = new Network(0, new Example(), 255);

		// reading the speed if
		if (args.length > 0) {
			try {
				speed = Integer.parseInt(args[0]);
			} catch (NumberFormatException e) {
				System.out.println("the speed must be an integer\n");
				System.exit(1);
			}
		}

		// initializing reader from command line
		int i, inp_num = 0;
		String input;
		BufferedReader in_stream = new BufferedReader(new InputStreamReader(
				System.in));

		// getting a list of the available serial ports
		Vector<String> ports = network.getPortList();

		// choosing the port to connect to
		System.out.println();
		if (ports.size() > 0) {
			System.out
					.println("the following serial ports have been detected:");
		} else {
			System.out
					.println("sorry, no serial ports were found on your computer\n");
			System.exit(0);
		}
		for (i = 0; i < ports.size(); ++i) {
			System.out.println("    " + Integer.toString(i + 1) + ":  "
					+ ports.elementAt(i));
		}
		boolean valid_answer = false;
		while (!valid_answer) {
			System.out
					.println("enter the id (1,2,...) of the connection to connect to: ");
			try {
				input = in_stream.readLine();
				inp_num = Integer.parseInt(input);
				if ((inp_num < 1) || (inp_num >= ports.size() + 1))
					System.out.println("your input is not valid");
				else
					valid_answer = true;
			} catch (NumberFormatException ex) {
				System.out.println("please enter a correct number");
			} catch (IOException e) {
				System.out.println("there was an input error\n");
				System.exit(1);
			}
		}

		// connecting to the selected port
		if (network.connect(ports.elementAt(inp_num - 1), speed)) {
			System.out.println();
		} else {
			System.out.println("sorry, there was an error connecting\n");
			System.exit(1);
		}

		// asking whether user wants to mirror traffic
		System.out
				.println("do you want this tool to send back all the received messages?");
		valid_answer = false;
		while (!valid_answer) {
			System.out.println("'y' for yes or 'n' for no: ");
			try {
				input = in_stream.readLine();
				if (input.equals("y")) {
					resend_active = true;
					valid_answer = true;
				} else if (input.equals("n")) {
					valid_answer = true;
				} else if (input.equals("q")) {
					System.out.println("example terminated\n");
					System.exit(0);
				}
			} catch (IOException e) {
				System.out.println("there was an input error\n");
				System.exit(1);
			}
		}

		// reading in numbers (bytes) to be sent over the serial port
		System.out.println("type 'q' to end the example");
		while (true) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e1) {
			}
			System.out
					.println("\nenter a number between 0 and 254 to be sent ('q' to exit): ");
			try {
				input = in_stream.readLine();
				if (input.equals("q")) {
					System.out.println("example terminated\n");
					network.disconnect();
					System.exit(0);
				}
				inp_num = Integer.parseInt(input);
				if ((inp_num > 255) || (inp_num < 0)) {
					System.out.println("the number you entered is not valid");
				} else {
					int temp[] = { inp_num };
					network.writeSerial(1, temp);
					System.out.println("sent " + inp_num + " over the serial port");
				}
			} catch (NumberFormatException ex) {
				System.out.println("please enter a correct number");
			} catch (IOException e) {
				System.out.println("there was an input error");
			}
		}
	    } catch (UnsatisfiedLinkError ule) {
	        System.out.println("rxtxSerial is not on the java.library.path = " + System.getProperty("java.library.path"));
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
	 * received bytes are written to command line (0 to 254) and the message is
	 * sent back over the same serial port.
	 * 
	 * @see net.Network_iface
	 */
	public void parseInput(int id, int numBytes, int[] message) {
		if (resend_active) {
			network.writeSerial(numBytes, message);
			System.out.print("received and sent back the following message: ");
		} else {
			System.out.print("received the following message: ");
		}
		System.out.print(message[0]);
		for (int i = 1; i < numBytes; ++i) {
			System.out.print(", ");
			System.out.print(message[i]);
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
