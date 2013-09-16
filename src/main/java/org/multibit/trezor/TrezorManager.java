/**
 * Copyright 2013 multibit.org
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
package org.multibit.trezor;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;

import java.util.ArrayList;
import java.util.Collection;


/**
 * Class managing Trezor hardware devices that are plugged in to the computer on
 * which MultiBit is running.
 *
 * This class has responsibility for tracking the Trezor devices being plugged in and removed.
 * It is also responsible for message passing to the device(s).
 */
public enum TrezorManager {
    INSTANCE;

    private BitcoinController controller;
    private MultiBitFrame mainFrame;

    /**
     * The collection of trezor devices that are currently connected.
     */
    private Collection<Trezor> trezors;

    private Logger log = LoggerFactory.getLogger(TrezorManager.class);

    public void initialise(BitcoinController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        trezors = new ArrayList<Trezor>();
    }


    /**
     * Get the URL contents in a background thread and check the version.
     */
//    private void checkVersionInBackground() {
//        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
//
//            private String message = null;
//            private final BitcoinController finalController = controller;
//
//            private StringBuffer stringBuffer = new StringBuffer();
//
//            @Override
//            protected Boolean doInBackground() throws Exception {
//                try {
//                    // Get the version file.
//                    final URL url = new URL(versionUrlToGet + "?version=" + finalController.getLocaliser().getVersionNumber());
//
//                    InputStream in = url.openStream();
//
//                    byte[] buffer = new byte[256];
//
//                    while (true) {
//                        int byteRead = in.read(buffer);
//                        if (byteRead == -1)
//                            break;
//                        for (int i = 0; i < byteRead; i++) {
//                            stringBuffer.append((char) buffer[i]);
//                        }
//                    }
//                    return true;
//                } catch (IOException ioe) {
//                    message = ioe.getClass().getCanonicalName() + " " + ioe.getMessage();
//                    return false;
//                }
//            }
//
//            @Override
//            protected void done() {
//                Boolean wasSuccessful = false;
//                try {
//                    wasSuccessful = get();
//
//                    if (wasSuccessful) {
//                        String result = stringBuffer.toString();
//                        if (result != null && result.length() > 0) {
//                            ParseResult parseResult = parseAndCheckVersionText(result);
//
//                            if (parseResult.isNewVersionIsAvailable()) {
//                                // See if we have already seen the new version.
//                                String alertManagerNewVersionValue = controller.getModel().getUserPreference(
//                                        BitcoinModel.ALERT_MANAGER_NEW_VERSION_VALUE);
//                                String alertManagerNewVersionSeenCount = controller.getModel().getUserPreference(
//                                        BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT);
//                                int seenCount = 0;
//
//                                if (alertManagerNewVersionSeenCount != null && alertManagerNewVersionSeenCount.trim().length() > 0) {
//                                    try {
//                                        seenCount = Integer.parseInt(alertManagerNewVersionSeenCount);
//                                    } catch (NumberFormatException nfe) {
//                                        // Reset count to zero.
//                                        controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
//                                                "0");
//                                    }
//                                }
//
//                                if (parseResult.getVersionOnServer() != null) {
//                                    if (parseResult.getVersionOnServer().equals(alertManagerNewVersionValue)) {
//                                        // We have seen this version before -
//                                        // use the number of times the user has
//                                        // been alerted
//                                        // that was previously stored in the
//                                        // user properties.
//                                    } else {
//                                        // Reset the number of times the alert
//                                        // has been shown.
//                                        seenCount = 0;
//                                    }
//                                }
//                                boolean showAlertDialog = seenCount < NUMBER_OF_TIMES_TO_REPEAT_ALERT;
//
//                                if (parseResult.getVersionOnServer() != null) {
//                                    controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_VALUE,
//                                            parseResult.getVersionOnServer());
//                                }
//                                seenCount++;
//                                controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
//                                        "" + seenCount);
//
//                                ImageIcon icon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
//                                String okText = controller.getLocaliser().getString("createOrEditAddressSubmitAction.text");
//                                String title = controller.getLocaliser().getString("alertManagerTitle");
//                                String line1 = controller.getLocaliser().getString("alertManagerLine1",
//                                        new String[] { parseResult.getVersionOnServer() });
//                                String line2 = controller.getLocaliser().getString("alertManagerLine2",
//                                        new String[] { parseResult.getLocalVersion() });
//                                String viewRelease = controller.getLocaliser().getString("alertManagerViewRelease");
//                                String[] choices;
//                                if (isBrowserSupported()) {
//                                    choices = new String[] { okText, viewRelease };
//                                } else {
//                                    choices = new String[] { okText };
//                                }
//
//                                ArrayList<String> messages = new ArrayList<String>();
//                                messages.add(line1);
//                                messages.add(line2);
//
//                                if (parseResult.getMessages() != null) {
//                                    // Add spacer.
//                                    messages.add(" ");
//                                    messages.addAll(parseResult.getMessages());
//                                }
//
//                                // Always put the alert message to the Messages window.
//                                Message separatorMessage = new Message(MESSAGE_WINDOW_SEPARATOR);
//                                separatorMessage.setShowInStatusBar(false);
//                                MessageManager.INSTANCE.addMessage(separatorMessage);
//                                for (String messageLine : messages) {
//                                    Message message = new Message(messageLine);
//                                    message.setShowInStatusBar(false);
//                                    MessageManager.INSTANCE.addMessage(message);
//                                }
//                                MessageManager.INSTANCE.addMessage(separatorMessage);
//
//                                if (showAlertDialog) {
//                                    int response = JOptionPane.showOptionDialog(mainFrame,
//                                            messages.toArray(new String[messages.size()]), title, JOptionPane.YES_NO_OPTION,
//                                            JOptionPane.PLAIN_MESSAGE, icon, choices, viewRelease);
//                                    if (response == 1) {
//                                        try {
//                                            openURI(new URI(RELEASES_URL));
//                                        } catch (URISyntaxException e) {
//                                            log.error(e.getClass().getCanonicalName() + " " + e.getMessage());
//                                        }
//                                    }
//                                }
//                            }
//                        } else {
//                            logUnableToLoadMessage(message);
//                        }
//                    } else {
//                        logUnableToLoadMessage(message);
//                    }
//                } catch (InterruptedException e) {
//                    logUnableToLoadMessage(message);
//                    e.printStackTrace();
//                } catch (ExecutionException e) {
//                    logUnableToLoadMessage(message);
//                    e.printStackTrace();
//                }
//            }
//        };
//        worker.execute();
//    }

    public Collection<Trezor> getTrezors() {
        return trezors;
    }
}