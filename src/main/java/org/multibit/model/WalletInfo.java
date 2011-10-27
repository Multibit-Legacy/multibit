package org.multibit.model;

import com.google.bitcoin.core.Address;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * wallet info is the companion info to the bitcoinj that multibit uses it
 * contains the sending and receiving addresses and the wallet version
 * 
 * it is stored in the same directory as the wallet and has the suffix ".info"
 * 
 * @author jim
 * 
 */
public class WalletInfo {

    private static final Logger log = LoggerFactory.getLogger(WalletInfo.class);

    /**
     * the total receiving addresses known - from the address book (will include
     * keys that are in other wallets)
     */
    private Set<AddressBookData> candidateReceivingAddresses;

    /**
     * the actual receiving addresses exposed for this address book (only keys
     * that occur in this wallet)
     */
    private Vector<AddressBookData> receivingAddresses;
    private Vector<AddressBookData> sendingAddresses;

    private static final String INFO_FILE_EXTENSION = "info";
    private static final String RECEIVE_ADDRESS_MARKER = "receive";
    private static final String SEND_ADDRESS_MARKER = "send";
    private static final String PROPERTY_MARKER = "property";
    private static final String SEPARATOR = ",";

    private static final String INFO_MAGIC_TEXT = "multiBit.info";
    private static final String INFO_VERSION_TEXT = "1";

    private static final String WALLET_VERSION_MARKER = "walletVersion";
    private static final String WALLET_VERSION_TEXT = "1";

    private String walletFilename;
    private String walletVersion;

    private Properties walletPreferences;

    /**
     * 
     * @param walletFilename
     *            the filename for the wallet this is the info for
     */
    public WalletInfo(String walletFilename) {
        this.walletFilename = walletFilename;

        candidateReceivingAddresses = new HashSet<AddressBookData>();
        // TODO Consider an ArrayList if possible
        receivingAddresses = new Vector<AddressBookData>();
        sendingAddresses = new Vector<AddressBookData>();

        walletPreferences = new Properties();

        loadFromFile();
    }

    public void put(String key, String value) {
        walletPreferences.put(key, value);
    }

    public String getProperty(String key) {
        return (String) walletPreferences.getProperty(key);
    }

    public Vector<AddressBookData> getReceivingAddresses() {
        return receivingAddresses;
    }

    public Vector<AddressBookData> getSendingAddresses() {
        return sendingAddresses;
    }

    public void setSendingAddresses(Vector<AddressBookData> sendingAddresses) {
        this.sendingAddresses = sendingAddresses;
    }

    /**
     * add a receiving address in the form of an AddressBookData, replacing the
     * label of any existing address
     * 
     * @param receivingAddress
     * @param addToCandidates
     *            - add to the list of candidate receiving addresses
     */
    public void addReceivingAddress(AddressBookData receivingAddress, boolean addToCandidates) {
        if (receivingAddress == null) {
            return;
        }

        Collection<AddressBookData> addressesToUse;
        if (addToCandidates) {
            addressesToUse = candidateReceivingAddresses;
        } else {
            addressesToUse = receivingAddresses;
        }

        boolean done = false;
        // check the address is not already in the set
        for (AddressBookData addressBookData : addressesToUse) {
            if (addressBookData.getAddress().equals(receivingAddress.getAddress())) {
                // just update label
                addressBookData.setLabel(receivingAddress.getLabel());
                done = true;
                break;
            }
        }

        if (!done) {
            addressesToUse.add(receivingAddress);
        }
    }

    /**
     * add a receiving address that belongs to a key of the current wallet this
     * will always be added and will take the label of any matching address in
     * the list of candidate addresses fron the address book
     * 
     * @param receivingAddress
     */
    public void addReceivingAddressOfKey(Address receivingAddress) {
        if (receivingAddress == null) {
            return;
        }

        if (!containsReceivingAddress(receivingAddress.toString())) {
            // see if there is a label in the candidate receiving addresses
            String label = "";

            for (AddressBookData addressBookData : candidateReceivingAddresses) {
                if (addressBookData.getAddress().equals(receivingAddress.toString())) {
                    label = addressBookData.getLabel();
                    break;
                }
            }
            receivingAddresses.add(new AddressBookData(label, receivingAddress.toString()));
        }
    }

    public boolean containsReceivingAddress(String receivingAddress) {
        boolean toReturn = false;
        // see if the receiving address is on the current list
        for (AddressBookData addressBookData : receivingAddresses) {
            if (addressBookData.getAddress().equals(receivingAddress.toString())) {
                // do nothing
                toReturn = true;
                break;
            }
        }

        return toReturn;
    }

    public void addSendingAddress(AddressBookData sendingAddress) {
        if (sendingAddress == null) {
            return;
        }

        boolean done = false;
        // check the address is not already in the Vector
        for (AddressBookData addressBookData : sendingAddresses) {
            if (addressBookData.getAddress() != null && addressBookData.getAddress().equals(sendingAddress.getAddress())) {
                // just update label
                addressBookData.setLabel(sendingAddress.getLabel());
                done = true;
                break;
            }
        }

        if (!done) {
            sendingAddresses.add(sendingAddress);
        }
    }

    public String lookupLabelForReceivingAddress(String address) {
        for (AddressBookData addressBookData : receivingAddresses) {
            if (addressBookData.getAddress().equals(address)) {
                return addressBookData.getLabel();
            }
        }

        return "";
    }

    public String lookupLabelForSendingAddress(String address) {
        for (AddressBookData addressBookData : sendingAddresses) {
            if (addressBookData.getAddress().equals(address)) {
                return addressBookData.getLabel();
            }
        }

        return "";
    }

    /**
     * write out the address book - a simple comma separated file format is used
     */
    public void writeToFile() {
        try {
            // we write out the union of the candidate and actual receiving
            // addresses
            HashMap<String, AddressBookData> allReceivingAddresses = new HashMap<String, AddressBookData>();
            if (candidateReceivingAddresses != null) {
                for (AddressBookData addressBookData : candidateReceivingAddresses) {
                    allReceivingAddresses.put(addressBookData.address, addressBookData);
                }
            }
            if (receivingAddresses != null) {
                for (AddressBookData addressBookData : receivingAddresses) {
                    allReceivingAddresses.put(addressBookData.address, addressBookData);
                }
            }

            // Create file
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(
                    createWalletInfoFilename(walletFilename)), "UTF8"));

            // write out the multibit addressbook identifier
            out.write(INFO_MAGIC_TEXT + SEPARATOR + INFO_VERSION_TEXT + "\n");

            // write out the wallet version
            out.write(WALLET_VERSION_MARKER + SEPARATOR + WALLET_VERSION_TEXT + "\n");

            Collection<AddressBookData> receiveAddressValues = allReceivingAddresses.values();
            for (AddressBookData addressBookData : receiveAddressValues) {
                String columnOne = RECEIVE_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + columnThree + "\n");
            }

            for (AddressBookData addressBookData : sendingAddresses) {
                String columnOne = SEND_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + columnThree + "\n");
            }

            for (Object key : walletPreferences.keySet()) {
                String columnOne = PROPERTY_MARKER;
                String columnTwo = (String) key;
                String columnThree = (String) walletPreferences.get(key);
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + columnThree + "\n");
            }

            // Close the output stream
            out.close();
        } catch (IOException ioe) {
            log.error(ioe.getMessage(), ioe);

        }
    }

    public void loadFromFile() {
        // TODO Is this try/catch block too wide in scope?
        try {
            walletPreferences = new Properties();

            // Read in the wallet info data
            FileInputStream fileInputStream = new FileInputStream(createWalletInfoFilename(walletFilename));
            // Get the object of DataInputStream
            InputStream inputStream = new DataInputStream(fileInputStream);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream, "UTF8"));
            String inputLine;

            // check the first line is what we expect
            String firstLine = bufferedReader.readLine();
            StringTokenizer tokenizer = new StringTokenizer(firstLine, SEPARATOR);
            int numberOfTokens = tokenizer.countTokens();
            if (numberOfTokens == 2) {
                String magicText = tokenizer.nextToken();
                String versionNumber = tokenizer.nextToken();
                if (!INFO_MAGIC_TEXT.equals(magicText) || !INFO_VERSION_TEXT.equals(versionNumber)) {
                    // this is not an multibit address book
                    return;
                }
            } else {
                // this is not an multibit address book
                return;
            }

            // read the wallet version
            String secondLine = bufferedReader.readLine();
            StringTokenizer walletVersionTokenizer = new StringTokenizer(secondLine, SEPARATOR);
            int walletVersionTokenNumber = walletVersionTokenizer.countTokens();
            if (walletVersionTokenNumber == 2) {
                String walletVersionMarker = walletVersionTokenizer.nextToken();
                String walletVersionString = walletVersionTokenizer.nextToken();
                if (!WALLET_VERSION_MARKER.equals(walletVersionMarker) || !WALLET_VERSION_TEXT.equals(walletVersionString)) {
                    // this refers to a version of the wallet we do not know
                    // about

                    // TODO throw exception
                    return;
                } else {
                    walletVersion = walletVersionString;
                }
            } else {
                // the format of the info format is wrong
                return;
            }

            // read the addresses and general properties
            boolean isMultilineColumnThree = false;
            String previousColumnOne = null;
            String previousColumnTwo = null;
            String multilineColumnThreeValue = null;
            
            while ((inputLine = bufferedReader.readLine()) != null) {
                if (inputLine.startsWith(RECEIVE_ADDRESS_MARKER) || inputLine.startsWith(SEND_ADDRESS_MARKER)
                        || inputLine.startsWith(PROPERTY_MARKER)) {
                    if (isMultilineColumnThree) {
                        // add previous multiline column three to model
                        if (RECEIVE_ADDRESS_MARKER.equals(previousColumnOne)) {
                            addReceivingAddress(new AddressBookData(multilineColumnThreeValue, previousColumnTwo), true);
                        } else {
                            if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                                addSendingAddress(new AddressBookData(multilineColumnThreeValue, previousColumnTwo));
                            } else {
                                if (PROPERTY_MARKER.equals(previousColumnOne)) {
                                    walletPreferences.put(previousColumnTwo, multilineColumnThreeValue);
                                }
                            }
                        }
                        previousColumnOne = null;
                        previousColumnTwo = null;
                        multilineColumnThreeValue = null;

                        isMultilineColumnThree = false;
                    }
                    StringTokenizer tokenizer2 = new StringTokenizer(inputLine, SEPARATOR);
                    int numberOfTokens2 = tokenizer2.countTokens();
                    String columnOne = null;
                    String columnTwo = null;
                    String columnThree = "";
                    if (numberOfTokens2 == 2) {
                        columnOne = tokenizer2.nextToken();
                        columnTwo = tokenizer2.nextToken();
                    } else {
                        if (numberOfTokens2 == 3) {
                            columnOne = tokenizer2.nextToken();
                            columnTwo = tokenizer2.nextToken();
                            columnThree = tokenizer2.nextToken();
                        }
                    }
                    if (RECEIVE_ADDRESS_MARKER.equals(columnOne)) {
                        addReceivingAddress(new AddressBookData(columnThree, columnTwo), true);
                    } else {
                        if (SEND_ADDRESS_MARKER.equals(columnOne)) {
                            addSendingAddress(new AddressBookData(columnThree, columnTwo));
                        } else {
                            if (PROPERTY_MARKER.equals(columnOne)) {
                                walletPreferences.put(columnTwo, columnThree);
                            }
                        }
                    }
                    
                    previousColumnOne = columnOne;
                    previousColumnTwo = columnTwo;
                    multilineColumnThreeValue = columnThree;
                } else {
                    // this is a multiline column 3 (typically a multiline
                    // label)
                    isMultilineColumnThree = true;
                    multilineColumnThreeValue = multilineColumnThreeValue + "\n" + inputLine;
                }
            }
            if (isMultilineColumnThree) {
                // add it to the model
                // add previous multiline column three to model
                if (RECEIVE_ADDRESS_MARKER.equals(previousColumnOne)) {
                    addReceivingAddress(new AddressBookData(multilineColumnThreeValue, previousColumnTwo), true);
                } else {
                    if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                        addSendingAddress(new AddressBookData(multilineColumnThreeValue, previousColumnTwo));
                    } else {
                        if (PROPERTY_MARKER.equals(previousColumnOne)) {
                            walletPreferences.put(previousColumnTwo, multilineColumnThreeValue);
                        }
                    }
                }
                previousColumnOne = null;
                previousColumnTwo = null;
                multilineColumnThreeValue = null;

                isMultilineColumnThree = false;
            }

            // Close the input stream
            inputStream.close();
        } catch (Exception e) {
            // Catch exception if any
            // may well not be a file - absorb exception
        }
    }

    /**
     * create wallet info filename
     * 
     * @param walletFilename
     */
    public static String createWalletInfoFilename(String walletFilename) {
        if (walletFilename == null) {
            return INFO_FILE_EXTENSION;
        }

        String walletInfoFilename = walletFilename;
        if (walletFilename.endsWith(MultiBitModel.WALLET_FILE_EXTENSION)) {
            walletInfoFilename = walletInfoFilename.substring(0,
                    walletFilename.length() - MultiBitModel.WALLET_FILE_EXTENSION.length() - 1);
            walletInfoFilename = walletInfoFilename + "." + INFO_FILE_EXTENSION;
        } else {
            walletInfoFilename = walletInfoFilename + "." + INFO_FILE_EXTENSION;
        }
        return walletInfoFilename;
    }

    public String getWalletVersion() {
        return walletVersion;
    }

    public String getWalletFilename() {
        return walletFilename;
    }
}
