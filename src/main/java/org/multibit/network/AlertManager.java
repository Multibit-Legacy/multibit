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
package org.multibit.network;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.ExecutionException;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.VersionComparator;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum AlertManager {
    INSTANCE;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private Logger log = LoggerFactory.getLogger(AlertManager.class);

    public static final String VERSION_URL = "https://multibit.org/version.txt";

    public static final String RELEASES_URL = "https://multibit.org/releases.html";

    public static final int NUMBER_OF_TIMES_TO_REPEAT_ALERT = 2;
    
    public static final String MESSAGE_WINDOW_SEPARATOR = "----------------------------------------------------------------";

    public static final String MESSAGE_PREFIX_TEXT = "message "; // one space

    public void initialise(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }

    /**
     * Check the version on the MultiBit server and show an alert dialog if
     * there is a newer version available.
     */
    public void checkVersion() {
        checkVersionInBackground();
    }

    /**
     * Get the URL contents in a background thread and check the version.
     */
    private void checkVersionInBackground() {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

            private String message = null;
            private final MultiBitController finalController = controller;

            private StringBuffer stringBuffer = new StringBuffer();

            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    // Get the version file.
                    final URL url = new URL(VERSION_URL + "?version=" + finalController.getLocaliser().getVersionNumber());

                    InputStream in = url.openStream();

                    byte[] buffer = new byte[256];

                    while (true) {
                        int byteRead = in.read(buffer);
                        if (byteRead == -1)
                            break;
                        for (int i = 0; i < byteRead; i++) {
                            stringBuffer.append((char) buffer[i]);
                        }
                    }
                    return true;
                } catch (IOException ioe) {
                    message = ioe.getClass().getCanonicalName() + " " + ioe.getMessage();
                    return false;
                }
            }

            @Override
            protected void done() {
                Boolean wasSuccessful = false;
                try {
                    wasSuccessful = get();

                    if (wasSuccessful) {
                        String result = stringBuffer.toString();
                        if (result != null && result.length() > 0) {
                            ParseResult parseResult = parseAndCheckVersionText(result);

                            if (parseResult.newVersionIsAvailable) {
                                // See if we have already seen the new version.
                                String alertManagerNewVersionValue = controller.getModel().getUserPreference(
                                        MultiBitModel.ALERT_MANAGER_NEW_VERSION_VALUE);
                                String alertManagerNewVersionSeenCount = controller.getModel().getUserPreference(
                                        MultiBitModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT);
                                int seenCount = 0;

                                if (alertManagerNewVersionSeenCount != null && alertManagerNewVersionSeenCount.trim().length() > 0) {
                                    try {
                                        seenCount = Integer.parseInt(alertManagerNewVersionSeenCount);
                                    } catch (NumberFormatException nfe) {
                                        // Reset count to zero.
                                        controller.getModel().setUserPreference(MultiBitModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
                                                "0");
                                    }
                                }

                                if (parseResult.versionOnServer != null) {
                                    if (parseResult.versionOnServer.equals(alertManagerNewVersionValue)) {
                                        // We have seen this version before -
                                        // use the number of times the user has
                                        // been alerted
                                        // that was previously stored in the
                                        // user properties.
                                    } else {
                                        // Reset the number of times the alert
                                        // has been shown.
                                        seenCount = 0;
                                    }
                                }
                                boolean showAlertDialog = seenCount < NUMBER_OF_TIMES_TO_REPEAT_ALERT;

                                if (parseResult.versionOnServer != null) {
                                    controller.getModel().setUserPreference(MultiBitModel.ALERT_MANAGER_NEW_VERSION_VALUE,
                                            parseResult.versionOnServer);
                                }
                                seenCount++;
                                controller.getModel().setUserPreference(MultiBitModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
                                        "" + seenCount);

                                ImageIcon icon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
                                String okText = controller.getLocaliser().getString("createOrEditAddressSubmitAction.text");
                                String title = controller.getLocaliser().getString("alertManagerTitle");
                                String line1 = controller.getLocaliser().getString("alertManagerLine1",
                                        new String[] { parseResult.versionOnServer });
                                String line2 = controller.getLocaliser().getString("alertManagerLine2",
                                        new String[] { parseResult.localVersion });
                                String viewRelease = controller.getLocaliser().getString("alertManagerViewRelease");
                                String[] choices;
                                if (isBrowserSupported()) {
                                    choices = new String[] { okText, viewRelease };
                                } else {
                                    choices = new String[] { okText };
                                }

                                ArrayList<String> messages = new ArrayList<String>();
                                messages.add(line1);
                                messages.add(line2);

                                if (parseResult.messages != null) {
                                    // Add spacer.
                                    messages.add(" ");
                                    messages.addAll(parseResult.messages);
                                }
                                
                                // Always put the alert message to the Messages window.
                                Message separatorMessage = new Message(MESSAGE_WINDOW_SEPARATOR);
                                separatorMessage.setShowInStatusBar(false);
                                MessageManager.INSTANCE.addMessage(separatorMessage);
                                for (String messageLine : messages) {
                                    Message message = new Message(messageLine);
                                    message.setShowInStatusBar(false);
                                    MessageManager.INSTANCE.addMessage(message);
                                }
                                MessageManager.INSTANCE.addMessage(separatorMessage);

                                if (showAlertDialog) {
                                    int response = JOptionPane.showOptionDialog(mainFrame,
                                            messages.toArray(new String[messages.size()]), title, JOptionPane.YES_NO_OPTION,
                                            JOptionPane.PLAIN_MESSAGE, icon, choices, viewRelease);
                                    if (response == 1) {
                                        try {
                                            openURI(new URI(RELEASES_URL));
                                        } catch (URISyntaxException e) {
                                            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
                                        }
                                    }
                                }
                            }
                        } else {
                            showUnableToLoadMessage(message);
                        }
                    } else {
                        showUnableToLoadMessage(message);
                    }
                } catch (InterruptedException e) {
                    showUnableToLoadMessage(message);
                    e.printStackTrace();
                } catch (ExecutionException e) {
                    showUnableToLoadMessage(message);
                    e.printStackTrace();
                }
            }
        };
        worker.execute();
    }

    /**
     * Parse the version text returned from the server and see if the version is
     * higher than the current MultiBit version. If so, return true.
     * 
     * @return true if version is later than the current version of MultiBit.
     * 
     *         The version file format is: <first line> Version of current
     *         MultiBit. <second line> message The first line of any message
     *         <third line> message The second line of any message <fourth line>
     *         message The third line of any message
     * 
     *         The version of the current version is anything that the
     *         VersionComparator can understand.
     */
    ParseResult parseAndCheckVersionText(String versionTextFromServer) {
        String versionOnServer = null;
        List<String> messageList = new ArrayList<String>();

        Scanner scanner = null;

        try {
            scanner = new Scanner(new StringReader(versionTextFromServer));

            // First line is the version
            if (scanner.hasNextLine()) {
                versionOnServer = scanner.nextLine();
            }

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line != null & line.startsWith(MESSAGE_PREFIX_TEXT)) {
                    // Extract message text
                    line = line.replaceFirst(MESSAGE_PREFIX_TEXT, "");
                    messageList.add(line);
                }
            }
        } finally {
            // Ensure the underlying stream is always closed
            // this only has any effect if the item passed
            // to the Scanner
            // constructor implements Closeable (which it
            // does in this case).
            if (scanner != null) {
                scanner.close();
            }
        }

        String localVersion = controller.getLocaliser().getVersionNumber();

        VersionComparator versionComparator = new VersionComparator();

        log.debug("Current version of local MultiBit = '" + localVersion + "', version on server = '" + versionOnServer + "'");
        boolean newVersionIsAvailable = versionComparator.compare(localVersion, versionOnServer) < 0;
        ParseResult parseResult = new ParseResult();
        parseResult.newVersionIsAvailable = newVersionIsAvailable;
        parseResult.localVersion = localVersion;
        parseResult.versionOnServer = versionOnServer;
        if (messageList.isEmpty()) {
            parseResult.messages = null;
        } else {
            parseResult.messages = messageList;
        }
        return parseResult;
    }

    private void showUnableToLoadMessage(String message) {
        if (message != null) {
            Message messageToShow = new Message(controller.getLocaliser().getString("browser.unableToLoad",
                    new String[] { VERSION_URL, message }), true);
            MessageManager.INSTANCE.addMessage(messageToShow);
        }
    }

    private boolean isBrowserSupported() {
        if (!java.awt.Desktop.isDesktopSupported()) {
            return false;
        }

        java.awt.Desktop desktop = java.awt.Desktop.getDesktop();

        if (!desktop.isSupported(java.awt.Desktop.Action.BROWSE)) {
            return false;
        }

        return true;
    }

    private void openURI(URI uri) {
        try {
            java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
            desktop.browse(uri);
        } catch (IOException ioe) {
            log.debug(ioe.getMessage());
            Message message = new Message(controller.getLocaliser().getString("browser.unableToLoad",
                    new String[] { uri.toString(), ioe.getMessage() }));
            MessageManager.INSTANCE.addMessage(message);
        }
    }

    class ParseResult {
        public boolean newVersionIsAvailable;
        public String versionOnServer;
        public String localVersion;
        public List<String> messages;

    }
}