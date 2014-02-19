/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

public final class ApplicationInstanceManager {

    private static final Logger log = LoggerFactory.getLogger(ApplicationInstanceManager.class);

    private static ApplicationInstanceListener subListener;

    /**
     * MultiDoge port number. Must be different to Multibit as specified in
     * http://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
     */
    public static final int MULTIBIT_NETWORK_SOCKET = 22554;

    /**
     * Multibit message start - must end with newline
     */
    public static final String MESSAGE_START = "$$MultiDogeMessageStart$$\n";

    /**
     * Multibit message end - must end with newline (nonsense text to make it
     * unlikely it appears in a URI)
     */
    public static final String MESSAGE_END = "\n$$X9Q3J7MessageEnd$$\n";

    private static Thread instanceListenerThread;

    private static boolean shutdownSocket = false;

    /**
     * Utility class should not have a public constructor
     */
    private ApplicationInstanceManager() {}

    /**
     * Registers this instance of the application. Passing in the rawURI that
     * was passed in on the command line
     *
     * @return true if first instance, false if not.
     */
    public static boolean registerInstance(String rawURI) {
        // returnValueOnError should be true if lenient (allows application to
        // run on network error) or false if strict.
        boolean returnValueOnError = true;
        // try to open network socket
        // if success, listen to socket for new instance message, return true
        // if unable to open, connect to existing and send new instance message,
        // return false
        try {
            final ServerSocket socket = new ServerSocket(MULTIBIT_NETWORK_SOCKET, 10, InetAddress.getByAddress(new byte[]{127, 0,
                0, 1}));
            log.debug("Listening for application instances on socket " + MULTIBIT_NETWORK_SOCKET);
            instanceListenerThread = new Thread(new Runnable() {
                @Override
                public void run() {
                    boolean socketClosed = false;
                    Socket client = null;
                    while (!socketClosed && !shutdownSocket) {
                        if (socket.isClosed()) {
                            socketClosed = true;
                        } else {
                            try {
                                client = socket.accept();
                                BufferedReader in = new BufferedReader(new InputStreamReader(client.getInputStream()));
                                String messageStart = in.readLine();
                                if (MESSAGE_START.trim().equals(messageStart.trim())) {
                                    log.debug("Message prefix matched - new application instance found");

                                    StringBuffer messageBody = new StringBuffer();
                                    boolean stillReading = true;
                                    boolean firstLine = true;
                                    while (stillReading) {
                                        String currentLine = in.readLine();
                                        if (currentLine == null) {
                                            // end of buffer
                                            stillReading = false;
                                        } else {
                                            if (MESSAGE_END.trim().equals(currentLine.trim())) {
                                                // end of message;
                                                stillReading = false;
                                            } else {
                                                if (!firstLine) {
                                                    messageBody.append("\n");
                                                }
                                                firstLine = false;
                                                // add message text
                                                messageBody.append(currentLine);
                                            }
                                        }
                                    }
                                    log.debug("rawURI extracted from message as '" + messageBody.toString() + "'");
                                    fireNewInstance(messageBody.toString());
                                }

                                in.close();
                                client.close();
                            } catch (IOException e) {
                                socketClosed = true;
                            }
                        }
                    }
                    // exited while due to shutdown request - shutdown socket

                    if (client != null) {
                        try {
                            client.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }

                    if (socket != null) {
                        try {
                            socket.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                    log.debug("Socket is shutdown.");
                }
            });
            instanceListenerThread.start();
            // listen
        } catch (UnknownHostException e) {
            log.error(e.getMessage(), e);
            return returnValueOnError;
        } catch (IOException e) {
            log.debug("Port is already taken.  Notifying first instance.");
            try {
                Socket clientSocket = new Socket(InetAddress.getByAddress(new byte[]{127, 0, 0, 1}), MULTIBIT_NETWORK_SOCKET);
                OutputStream out = clientSocket.getOutputStream();
                out.write(MESSAGE_START.getBytes());
                if (rawURI != null) {
                    out.write(rawURI.getBytes());
                }
                out.write(MESSAGE_END.getBytes());
                out.close();
                clientSocket.close();
                log.debug("Successfully notified first instance.");
                return false;
            } catch (UnknownHostException e1) {
                log.error(e.getMessage(), e);
                return returnValueOnError;
            } catch (IOException e1) {
                log.error("Error connecting to local port for single instance notification");
                log.error(e1.getMessage(), e1);
                return returnValueOnError;
            }
        }
        return true;
    }

    public static void setApplicationInstanceListener(ApplicationInstanceListener listener) {
        subListener = listener;
    }

    private static void fireNewInstance(String rawURI) {
        if (subListener != null) {
            subListener.newInstanceCreated(rawURI);
        }
    }

    public static void shutdownSocket() {
        log.debug("Making request to shut down socket ...");

        shutdownSocket = true;
    }
}
