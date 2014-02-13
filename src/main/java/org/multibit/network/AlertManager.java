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
package org.multibit.network;

import java.io.BufferedReader;
import java.io.Console;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
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

import org.multibit.Localiser;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.core.CoreModel;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.VersionComparator;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import com.google.dogecoin.core.ECKey;
import com.google.dogecoin.core.NetworkParameters;
import com.google.dogecoin.core.Utils;
import com.google.dogecoin.core.Wallet;



public enum AlertManager {
    INSTANCE;

    private BitcoinController controller;
    private MultiBitFrame mainFrame;

    private Logger log = LoggerFactory.getLogger(AlertManager.class);

    public static final String DEFAULT_VERSION_URL = "http://multidoge.org/version.txt";

    public static final String RELEASES_URL = "http://multidoge.org/";

    public static final int NUMBER_OF_TIMES_TO_REPEAT_ALERT = 3;
    
    public static final String MESSAGE_WINDOW_SEPARATOR = "----------------------------------------------------------------";

    public static final String MESSAGE_PREFIX_TEXT = "message "; // one space
    
    public static final String SIGNATURE_PREFIX_TEXT = "signature "; // one space
    
    private String versionUrlToGet = DEFAULT_VERSION_URL;
    
    /**
     * The public keys of private keys used to sign the version/ alert messages.
     * These correspond to the following addresses (in order):
     *    multibit.org's key Bitcoin address = 1AhN6rPdrMuKBGFDKR1k9A8SCLYaNgXhty
     *    Tim Molter's key Bitcoin address = 1LMwcJiCjqHS1JVGEokUNwYJHfdPgudA8e
     *    Gary Rowe's key Bitcoin address =
     */
    public String[] WHITELIST_PUBLIC_KEYS = new String[]{};
    
    public void initialise(BitcoinController controller, MultiBitFrame mainFrame) {
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
            private final BitcoinController finalController = controller;

            private StringBuffer stringBuffer = new StringBuffer();

            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    // Get the version file.
                    final URL url = new URL(versionUrlToGet + "?version=" + finalController.getLocaliser().getVersionNumber());

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

                            if (parseResult.isNewVersionIsAvailable()) {
                                // See if we have already seen the new version.
                                String alertManagerNewVersionValue = controller.getModel().getUserPreference(
                                        BitcoinModel.ALERT_MANAGER_NEW_VERSION_VALUE);
                                String alertManagerNewVersionSeenCount = controller.getModel().getUserPreference(
                                        BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT);
                                int seenCount = 0;

                                if (alertManagerNewVersionSeenCount != null && alertManagerNewVersionSeenCount.trim().length() > 0) {
                                    try {
                                        seenCount = Integer.parseInt(alertManagerNewVersionSeenCount);
                                    } catch (NumberFormatException nfe) {
                                        // Reset count to zero.
                                        controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
                                                "0");
                                    }
                                }

                                if (parseResult.getVersionOnServer() != null) {
                                    if (parseResult.getVersionOnServer().equals(alertManagerNewVersionValue)) {
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

                                if (parseResult.getVersionOnServer() != null) {
                                    controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_VALUE,
                                            parseResult.getVersionOnServer());
                                }
                                seenCount++;
                                controller.getModel().setUserPreference(BitcoinModel.ALERT_MANAGER_NEW_VERSION_SEEN_COUNT,
                                        "" + seenCount);

                                ImageIcon icon = ImageLoader.createImageIcon(ImageLoader.MULTIBIT_ICON_FILE);
                                String okText = controller.getLocaliser().getString("createOrEditAddressSubmitAction.text");
                                String title = controller.getLocaliser().getString("alertManagerTitle");
                                String line1 = controller.getLocaliser().getString("alertManagerLine1",
                                        new String[] { parseResult.getVersionOnServer() });
                                String line2 = controller.getLocaliser().getString("alertManagerLine2",
                                        new String[] { parseResult.getLocalVersion() });
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

                                if (parseResult.getMessages() != null) {
                                    // Add spacer.
                                    messages.add(" ");
                                    messages.addAll(parseResult.getMessages());
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
                            logUnableToLoadMessage(message);
                        }
                    } else {
                        logUnableToLoadMessage(message);
                    }
                } catch (InterruptedException e) {
                    logUnableToLoadMessage(message);
                    e.printStackTrace();
                } catch (ExecutionException e) {
                    logUnableToLoadMessage(message);
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
    public ParseResult parseAndCheckVersionText(String versionTextFromServer) {
        String versionOnServer = null;
        List<String> messages = new ArrayList<String>();
        List<Signature> signatures = new ArrayList<Signature>();

        Scanner scanner = null;

        try {
            scanner = new Scanner(new StringReader(versionTextFromServer));

            // First line is the version
            if (scanner.hasNextLine()) {
                versionOnServer = scanner.nextLine();
            }

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line != null) {
                    if (line.startsWith(MESSAGE_PREFIX_TEXT)) {
                        // Extract message text.
                        line = line.replaceFirst(MESSAGE_PREFIX_TEXT, "");
                        messages.add(line);
                    } else {
                        if (line.startsWith(SIGNATURE_PREFIX_TEXT)) {
                            // Extract signature public key and signature text
                            String[] tokens = line.split("[ ]+");
                            // There should be 3 tokens : signature <public key> <signature text>.
                            if (tokens != null && tokens.length == 3) {
                                Signature signature = new Signature();
                                signature.setPublicKeyAsHex(tokens[1]);
                                signature.setSignatureText(tokens[2]);
                                signatures.add(signature);
                            }
                        }   
                    }
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
        parseResult.setNewVersionIsAvailable(newVersionIsAvailable);
        parseResult.setLocalVersion(localVersion);
        parseResult.setVersionOnServer(versionOnServer);
        parseResult.setMessages(messages);
        parseResult.setSignatures(signatures);
        
        return parseResult;
    }

    private void logUnableToLoadMessage(String message) {
        if (message != null) {
            log.error(controller.getLocaliser().getString("browser.unableToLoad", new String[] { versionUrlToGet, message }));
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

    public String getVersionUrlToGet() {
        return versionUrlToGet;
    }

    public void setVersionUrlToGet(String versionUrlToGet) {
        this.versionUrlToGet = versionUrlToGet;
    }
    
    /**
     * Main method to sign a file with a specific key.
     * The key is assumed to be in an encrypted wallet so you need to specify a password.
     * 
     * The usage is:
     * 1) Store the file you want to sign, say c:/version.txt
     * 2) Say your signing key is stored in a wallet c:/myWallets/signing.wallet, the address of your signing key starts with 1ABC123 and your password is "dog".
     * 3) Invoke the main on the command line with:
     * 
     * > java -jar multbit-exe.jar c:/version.txt c:/myWallets/signing.wallet 1ABC123 dog
     * 
     * The version.txt file will have the correct signature appended to it.
     * 
     */
    public static void main(String[] args) {
        // Usage note.
        if(args == null || args.length < 3) {
            System.out.println("Usage: java -cp  multibit-exe.jar org.multibit.network.AlertManager <file to sign> <wallet location> <address to specify private key> [password]");
            System.out.println("Usage: if you omit your password you will be prompted for it.");
            System.exit(0);
        }
        
        String fileToSignString = args[0];
        String walletLocation = args[1];
        String addressPrefix = args[2];
        
        String password = null;
        if (args.length == 4) {
            password = args[3];
        } else {
            Console c = System.console();
            if (c == null) {
                System.err.println("No console.");
                System.exit(1);
            }

            password = new String(c.readPassword("Enter your wallet password: "));
        }
        
        // Initialise a few things.
        final CoreController controller = new CoreController();
        final BitcoinController bitcoinController = new BitcoinController(controller);
        
        final Localiser localiser = new Localiser();
        final CoreModel coreModel = new CoreModel();
        final BitcoinModel model = new BitcoinModel(coreModel);
        
        controller.setLocaliser(localiser);
        controller.setModel(coreModel);
        bitcoinController.setModel(model);
        
        // Initialise and check
        AlertManager alertManager = AlertManager.INSTANCE;        
        alertManager.initialise(bitcoinController, null);

        FileWriter fileWriter = null;
        try {
            // Get the text to sign.
            File fileToSign = new File(fileToSignString);
            String textToSign = readFile(fileToSign);
            
            // Load up the wallet containing the signing key.
            File walletFile = new File(walletLocation);
            FileHandler fileHandler = new FileHandler(bitcoinController);
            WalletData perWalletModelData = fileHandler.loadFromFile(walletFile);
            
            // Find the private key whose Bitcoin address matches the passed in addressPrefix.
            ECKey signingKey = null;
            Wallet wallet = perWalletModelData.getWallet();
            List<ECKey> keys = wallet.getKeychain();
            for (ECKey key : keys) {
                if (key.toAddress(NetworkParameters.prodNet()).toString().toLowerCase().startsWith(addressPrefix.toLowerCase())) {
                    // This is the signing key.
                    signingKey = key;
                    break;
                }
            }
            
            if (signingKey == null) {
                System.out.println("No signing key could be found with the Bitcoin address prefix of '" + addressPrefix + "'");
            }
            
            String publicKeyAsHex = Utils.bytesToHexString(signingKey.getPubKey());

            KeyParameter keyParameter = wallet.getKeyCrypter().deriveKey(password);
            ECKey decryptedSigningKey = signingKey.decrypt(wallet.getKeyCrypter(), keyParameter);
            String signatureText = decryptedSigningKey.signMessage(textToSign, keyParameter);
            
            fileWriter = new FileWriter(fileToSignString, true); // The true will append the new data
            fileWriter.write(SIGNATURE_PREFIX_TEXT + publicKeyAsHex + " " + signatureText + "\n");
            fileWriter.close();
            
            System.out.println("SUCCESS. The signature for the private key with address " + signingKey.toAddress(NetworkParameters.prodNet()) + " has been appended to " + fileToSignString);
            
        } catch (IOException ioe) {
            ioe.printStackTrace();
        } finally {
            if (fileWriter != null) {
                try {
                    fileWriter.close();
                } catch (IOException e) {

                    e.printStackTrace();
                }
            }
        }
    }
    
    private static String readFile(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        StringBuilder stringBuilder = new StringBuilder();
        while ((line = reader.readLine()) != null) {
            // We do not want to sign other signatures.
            if (!line.startsWith(SIGNATURE_PREFIX_TEXT)) {
                stringBuilder.append(line).append("\n");
            }
        }
        return stringBuilder.toString();
    }
}