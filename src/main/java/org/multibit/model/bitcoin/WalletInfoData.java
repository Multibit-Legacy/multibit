/**
 * Copyright 2012 multibit.org
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
package org.multibit.model.bitcoin;

import com.google.dogecoin.core.Address;
import com.google.dogecoin.core.ECKey;
import com.google.dogecoin.core.Wallet;
import org.multibit.MultiBit;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;

/**
 * Wallet info is the companion info to the bitcoinj Wallet that multibit uses
 * it contains the sending and receiving addresses and the wallet version.
 * 
 * It is stored in the same directory as the wallet and has the suffix ".info".
 * 
 * @author jim
 * 
 */
public class WalletInfoData {

    private static final Logger log = LoggerFactory.getLogger(WalletInfoData.class);
    private static final String ENCODED_SPACE_CHARACTER = "%20";
    private static final String VALID_HEX_CHARACTERS = "01234567890abcdef";

    /**
     * The actual receiving addresses exposed for this address book (only keys
     * that occur in this wallet).
     */
    private ArrayList<WalletAddressBookData> receivingAddresses;
    private ArrayList<WalletAddressBookData> sendingAddresses;

    private static final String INFO_FILE_EXTENSION = "info";
    private static final String RECEIVE_ADDRESS_MARKER = "receive";
    private static final String SEND_ADDRESS_MARKER = "send";
    private static final String PROPERTY_MARKER = "property";
    private static final String SEPARATOR = ",";

    private static final String INFO_MAGIC_TEXT = "multiBit.info";
    private static final String INFO_VERSION_TEXT = "1";

    private static final String WALLET_VERSION_MARKER = "walletVersion";

    public static final String DESCRIPTION_PROPERTY = "walletDescription";
    public static final String SIZE_PROPERTY = "walletSize";
    public static final String DATE_LAST_MODIFED_PROPERTY = "walletLastModified";

    private String walletFilename;
    private MultiBitWalletVersion walletVersion;
    private Wallet wallet;

    private Properties walletPreferences;

    /**
     * Flag indicated that the wallet has been deleted and should not be used.
     */
    private boolean deleted = false;

    /**
     * 
     * @param walletFilename
     *            the filename for the wallet this is the info for
     */
    public WalletInfoData(String walletFilename, Wallet wallet, MultiBitWalletVersion walletVersion) {
        this.walletFilename = walletFilename;
        this.walletVersion = walletVersion;
        this.wallet = wallet;

        receivingAddresses = new ArrayList<WalletAddressBookData>();
        sendingAddresses = new ArrayList<WalletAddressBookData>();

        walletPreferences = new Properties();

        try {
            loadFromFile();
        } catch (WalletLoadException wle) {
            // load may fail on first construct if no backing file written -
            // just log it unless it is from wallet version
            log.debug(wle.getMessage());
            if (wle.getCause() != null) {
                log.debug("Cause : " + wle.getCause().getMessage());
            }
        }
    }

    /**
     * Put a String property into the walletinfo.
     * 
     * @param key
     * @param value
     */
    public void put(String key, String value) {
        walletPreferences.put(key, value);
    }

    /**
     * Get a String property from the wallet info.
     * 
     * @param key
     * @return
     */
    public String getProperty(String key) {
        return walletPreferences.getProperty(key);
    }

    /**
     * Removes a property from the wallet info.
     * 
     * @param key
     */
    public void remove(String key) {
        walletPreferences.remove(key);
    }

    public ArrayList<WalletAddressBookData> getReceivingAddresses() {
        return receivingAddresses;
    }

    public ArrayList<WalletAddressBookData> getSendingAddresses() {
        return sendingAddresses;
    }

    public void setReceivingAddresses(ArrayList<WalletAddressBookData> receivingAddresses) {
        this.receivingAddresses = receivingAddresses;
    }

    /**
     * Add a receiving address in the form of an WalletAddressBookData,
     * replacing the label of any existing address.
     * 
     * @param receivingAddress
     * @param checkAlreadyPresent
     */
    public void addReceivingAddress(WalletAddressBookData receivingAddress, boolean checkAlreadyPresent) {
        if (receivingAddress == null || receivingAddress.getAddress() == null) {
            return;
        }

        boolean justUpdateLabel = false;

        if (checkAlreadyPresent) {
            // Check the address is not already in the set.
            for (WalletAddressBookData addressBookData : receivingAddresses) {
                if (addressBookData.getAddress().equals(receivingAddress.getAddress())) {
                    // Just update label.
                    addressBookData.setLabel(receivingAddress.getLabel());
                    justUpdateLabel = true;
                    break;
                }
            }
        }

        boolean addressMatchesKey = false;
        if (wallet != null) {
            for (ECKey key : wallet.getKeys()) {
                if (receivingAddress.getAddress().equals(
                        key.toAddress(MultiBit.getBitcoinController().getModel().getNetworkParameters()).toString())) {
                    addressMatchesKey = true;
                    break;
                }
            }
        }

        if (!justUpdateLabel && (wallet == null || addressMatchesKey)) {
            receivingAddresses.add(receivingAddress);
        }
    }

    /**
     * Ensure only receiving addresses actually in the wallet appear. This is to
     * prevent adding receiving addresses manually in the info file.
     */
    public void checkAllReceivingAddressesAppearInWallet(Wallet wallet) {
        List<WalletAddressBookData> toRemove = new ArrayList<WalletAddressBookData>();
        if (wallet != null) {
            Iterator<WalletAddressBookData> iterator = receivingAddresses.iterator();
            while (iterator.hasNext()) {
                boolean addressMatchesKey = false;
                WalletAddressBookData walletAddressBookData = iterator.next();
                for (ECKey key : wallet.getKeys()) {
                    if (walletAddressBookData.getAddress().equals(
                            key.toAddress(MultiBit.getBitcoinController().getModel().getNetworkParameters()).toString())) {
                        addressMatchesKey = true;
                        break;
                    }
                }
                
                if (!addressMatchesKey) {
                    // Remove from receivingAddresses and log.
                    toRemove.add(walletAddressBookData);
                    log.debug("Removed receiving address " + walletAddressBookData.getAddress() + " because it did not match a key in the wallet '" + wallet.getDescription() + "'");
                }
            }
            
            receivingAddresses.removeAll(toRemove);
        }
    }

    /**
     * Add a receiving address that belongs to a key of the current wallet this
     * will always be added and will take the label of any matching address in
     * the list of candidate addresses from the address book.
     * 
     * @param receivingAddress
     */
    public void addReceivingAddressOfKey(Address receivingAddress) {
        if (receivingAddress == null) {
            return;
        }

        if (!containsReceivingAddress(receivingAddress.toString())) {
            receivingAddresses.add(new WalletAddressBookData("", receivingAddress.toString()));
        }
    }

    public boolean containsReceivingAddress(String receivingAddress) {
        boolean toReturn = false;
        // see if the receiving address is on the current list
        for (WalletAddressBookData addressBookData : receivingAddresses) {
            if (addressBookData.getAddress().equals(receivingAddress.toString())) {
                // do nothing
                toReturn = true;
                break;
            }
        }

        return toReturn;
    }

    public void addSendingAddress(WalletAddressBookData sendingAddress) {
        if (sendingAddress == null) {
            return;
        }

        boolean done = false;
        // Check the address is not already in the arraylist.
        for (WalletAddressBookData addressBookData : sendingAddresses) {
            if (addressBookData.getAddress() != null && addressBookData.getAddress().equals(sendingAddress.getAddress())) {
                // Just update label.
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
        for (WalletAddressBookData addressBookData : receivingAddresses) {
            if (addressBookData.getAddress().equals(address)) {
                return addressBookData.getLabel();
            }
        }

        return "";
    }

    public String lookupLabelForSendingAddress(String address) {
        for (WalletAddressBookData addressBookData : sendingAddresses) {
            if (addressBookData.getAddress().equals(address)) {
                return addressBookData.getLabel();
            }
        }

        return "";
    }

    /**
     * Write out the wallet info to the file specified as a parameter - a comma
     * separated file format is used.
     * 
     * @param walletInfoFilename
     *            The full path of the wallet info file to write
     * @param walletVersion
     *            The wallet version.
     * @throws WalletSaveException
     *             Exception if write is unsuccessful
     */
    public void writeToFile(String walletInfoFilename, MultiBitWalletVersion walletVersion) throws WalletSaveException {
        BufferedWriter out = null;
        try {
            // We write out all the receiving addresses.
            LinkedHashMap<String, WalletAddressBookData> allReceivingAddresses = new LinkedHashMap<String, WalletAddressBookData>();
            if (receivingAddresses != null) {
                for (WalletAddressBookData addressBookData : receivingAddresses) {
                    allReceivingAddresses.put(addressBookData.address, addressBookData);
                }
            }

            // Create file.
            out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(walletInfoFilename), "UTF8"));

            // Write out the multibit addressbook identifier.
            out.write(INFO_MAGIC_TEXT + SEPARATOR + INFO_VERSION_TEXT + "\n");

            // Write out the wallet version.
            out.write(WALLET_VERSION_MARKER + SEPARATOR + walletVersion.getWalletVersionString() + "\n");

            Collection<WalletAddressBookData> receiveAddressValues = allReceivingAddresses.values();
            for (WalletAddressBookData addressBookData : receiveAddressValues) {
                String columnOne = RECEIVE_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + encodeURLString(columnThree) + "\n");
            }

            for (WalletAddressBookData addressBookData : sendingAddresses) {
                String columnOne = SEND_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + encodeURLString(columnThree) + "\n");
            }

            // Remove some properties form the wallet file that dont need to be persisted.
            Properties walletPreferencesClone = new Properties();
            walletPreferencesClone.putAll(walletPreferences);
            walletPreferencesClone.remove(BitcoinModel.WALLET_FILE_SIZE);
            walletPreferencesClone.remove(BitcoinModel.WALLET_FILE_LAST_MODIFIED);
            walletPreferencesClone.remove(BitcoinModel.WALLET_INFO_FILE_SIZE);
            walletPreferencesClone.remove(BitcoinModel.WALLET_INFO_FILE_LAST_MODIFIED);

            walletPreferencesClone.remove(BitcoinModel.VALIDATION_ADDRESS_IS_INVALID);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_ADDRESS_VALUE);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_AMOUNT_IS_INVALID);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_AMOUNT_IS_MISSING);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_AMOUNT_VALUE);
            walletPreferencesClone.remove(BitcoinModel.VALIDATION_NOT_ENOUGH_FUNDS);

            walletPreferencesClone.remove(BitcoinModel.SEND_PERFORM_PASTE_NOW);
            
            // These properties are obselete so removed from the info file to tidy them up.
            walletPreferencesClone.remove("sendErrorMessage");
            walletPreferencesClone.remove("sendWasSuccessful");
            walletPreferencesClone.remove("earliestTransactionDate");

            for (Map.Entry entry : walletPreferencesClone.entrySet()) {

                String columnOne = PROPERTY_MARKER;
                String columnTwo = (String) entry.getKey();
                String encodedColumnThree = encodeURLString((String) entry.getValue());
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + encodedColumnThree + "\n");
            }
        } catch (IOException ioe) {
            throw new WalletSaveException("Could not write walletinfo file for wallet '" + walletInfoFilename + "'", ioe);
        } finally {
            // Close the output stream.
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    throw new WalletSaveException("Could not close walletinfo file for wallet '" + walletInfoFilename + "'", e);
                }
            }
        }
    }

    /**
     * Load the internally referenced wallet info file.
     * 
     * @throws WalletInfoException
     *             Exception if read is unsuccessful
     */
    public void loadFromFile() {
        String walletInfoFilename = null;
        InputStream inputStream = null;
        try {
            walletPreferences = new Properties();

            // Read in the wallet info data.
            walletInfoFilename = createWalletInfoFilename(walletFilename);
            FileInputStream fileInputStream = new FileInputStream(walletInfoFilename);
            // Get the object of DataInputStream.
            inputStream = new DataInputStream(fileInputStream);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream, "UTF8"));
            String inputLine;

            // Check the first line is what we expect.
            String firstLine = bufferedReader.readLine();
            if (firstLine == null) {
                // This is not an multibit address book.
                throw new WalletLoadException("The file '" + walletInfoFilename
                        + "' is not a valid wallet info file (empty line 1)");
            }
            StringTokenizer tokenizer = new StringTokenizer(firstLine, SEPARATOR);
            int numberOfTokens = tokenizer.countTokens();
            if (numberOfTokens == 2) {
                String magicText = tokenizer.nextToken();
                String versionNumber = tokenizer.nextToken();
                if (!INFO_MAGIC_TEXT.equals(magicText) || !INFO_VERSION_TEXT.equals(versionNumber)) {
                    // This is not an multibit address book.
                    throw new WalletLoadException("The file '" + walletInfoFilename
                            + "' is not a valid wallet info file (wrong magic number on line 1)");
                }
            } else {
                // This is not an multibit address book.
                throw new WalletLoadException("The file '" + walletInfoFilename
                        + "' is not a valid wallet info file (wrong number of tokens on line 1)");
            }

            // Read the wallet version.
            String secondLine = bufferedReader.readLine();
            StringTokenizer walletVersionTokenizer = new StringTokenizer(secondLine, SEPARATOR);
            int walletVersionTokenNumber = walletVersionTokenizer.countTokens();
            if (walletVersionTokenNumber == 2) {
                String walletVersionMarker = walletVersionTokenizer.nextToken();
                String walletVersionString = walletVersionTokenizer.nextToken();
                if (!WALLET_VERSION_MARKER.equals(walletVersionMarker)
                        || !(MultiBitWalletVersion.SERIALIZED.getWalletVersionString().equals(walletVersionString)
                                || MultiBitWalletVersion.PROTOBUF.getWalletVersionString().equals(walletVersionString) || MultiBitWalletVersion.PROTOBUF_ENCRYPTED
                                .getWalletVersionString().equals(walletVersionString))) {
                    // This refers to a version of the wallet we do not know
                    // about.
                    throw new WalletVersionException("Cannot understand wallet version of '" + walletVersionMarker + "', '"
                            + walletVersionString + "'");
                } else {
                    // The wallet version passed in the file is used rather than
                    // the value in the constructor
                    if (!walletVersion.getWalletVersionString().equals(walletVersionString)) {
                        log.debug("The wallet version in the constructor was '" + walletVersion
                                + "'. In the wallet info file it was '" + walletVersionString + "'. Using the latter.");
                        if (MultiBitWalletVersion.SERIALIZED.getWalletVersionString().equals(walletVersionString)) {
                            walletVersion = MultiBitWalletVersion.SERIALIZED;
                        } else if (MultiBitWalletVersion.PROTOBUF.getWalletVersionString().equals(walletVersionString)) {
                            walletVersion = MultiBitWalletVersion.PROTOBUF;
                        } else if (MultiBitWalletVersion.PROTOBUF_ENCRYPTED.getWalletVersionString().equals(walletVersionString)) {
                            walletVersion = MultiBitWalletVersion.PROTOBUF_ENCRYPTED;
                        }
                    }
                }
            } else {
                // The format of the info format is wrong.
                throw new WalletVersionException("Cannot understand wallet version text of '" + secondLine + "'");
            }

            // Read the addresses and general properties.
            boolean isMultilineColumnThree = false;
            String previousColumnOne = null;
            String previousColumnTwo = null;
            String multilineColumnThreeValue = null;

            while ((inputLine = bufferedReader.readLine()) != null) {
                if (inputLine.startsWith(RECEIVE_ADDRESS_MARKER + SEPARATOR)
                        || inputLine.startsWith(SEND_ADDRESS_MARKER + SEPARATOR)
                        || inputLine.startsWith(PROPERTY_MARKER + SEPARATOR)) {
                    if (isMultilineColumnThree) {
                        // Add previous multiline column three to model.
                        String decodedMultiLineColumnThreeValue = decodeURLString(multilineColumnThreeValue);

                        if (RECEIVE_ADDRESS_MARKER.equals(previousColumnOne)) {
                            addReceivingAddress(new WalletAddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo),
                                    true);
                        } else {
                            if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                                addSendingAddress(new WalletAddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo));
                            } else {
                                if (PROPERTY_MARKER.equals(previousColumnOne)) {
                                    walletPreferences.put(previousColumnTwo, decodedMultiLineColumnThreeValue);
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
                    String decodedColumnThreeValue = decodeURLString(columnThree);
                    if (RECEIVE_ADDRESS_MARKER.equals(columnOne)) {
                        addReceivingAddress(new WalletAddressBookData(decodedColumnThreeValue, columnTwo), true);
                    } else {
                        if (SEND_ADDRESS_MARKER.equals(columnOne)) {
                            addSendingAddress(new WalletAddressBookData(decodedColumnThreeValue, columnTwo));
                        } else {
                            if (PROPERTY_MARKER.equals(columnOne)) {
                                walletPreferences.put(columnTwo, decodedColumnThreeValue);
                            }
                        }
                    }

                    previousColumnOne = columnOne;
                    previousColumnTwo = columnTwo;
                    multilineColumnThreeValue = columnThree;
                } else {
                    // This is a multiline column 3 (typically a multiline
                    // label).
                    isMultilineColumnThree = true;
                    multilineColumnThreeValue = multilineColumnThreeValue + "\n" + inputLine;
                }
            }
            if (isMultilineColumnThree) {
                // Add previous multiline column three to model.
                String decodedMultiLineColumnThreeValue = decodeURLString(multilineColumnThreeValue);
                if (RECEIVE_ADDRESS_MARKER.equals(previousColumnOne)) {
                    addReceivingAddress(new WalletAddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo), true);
                } else {
                    if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                        addSendingAddress(new WalletAddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo));
                    } else {
                        if (PROPERTY_MARKER.equals(previousColumnOne)) {
                            walletPreferences.put(previousColumnTwo, decodedMultiLineColumnThreeValue);
                        }
                    }
                }
                previousColumnOne = null;
                previousColumnTwo = null;
                multilineColumnThreeValue = null;

                isMultilineColumnThree = false;
            }
        } catch (IllegalArgumentException iae) {
            throw new WalletLoadException("Could not load walletinfo file '" + walletInfoFilename + "'", iae);
        } catch (IOException ioe) {
            throw new WalletLoadException("Could not load walletinfo file '" + walletInfoFilename + "'", ioe);
        } finally {
            // Close the input stream
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    throw new WalletLoadException("Could not close walletinfo file '" + walletInfoFilename + "'", e);
                }
            }
        }
    }

    /**
     * Create wallet info filename.
     * 
     * @param walletFilename
     */
    public static String createWalletInfoFilename(String walletFilename) {
        if (walletFilename == null) {
            return INFO_FILE_EXTENSION;
        }

        String walletInfoFilename = walletFilename;
        if (walletFilename.endsWith(BitcoinModel.WALLET_FILE_EXTENSION)) {
            walletInfoFilename = walletInfoFilename.substring(0,
                    walletFilename.length() - BitcoinModel.WALLET_FILE_EXTENSION.length() - 1);
            walletInfoFilename = walletInfoFilename + "." + INFO_FILE_EXTENSION;
        } else {
            walletInfoFilename = walletInfoFilename + "." + INFO_FILE_EXTENSION;
        }
        return walletInfoFilename;
    }

    /**
     * Encode a string using URL encoding.
     * 
     * @param stringToEncode
     *            The string to URL encode
     */
    public static String encodeURLString(String stringToEncode) {
        if (stringToEncode == null) {
            return "";
        }
        try {
            return java.net.URLEncoder.encode(stringToEncode, "UTF-8").replace("+", ENCODED_SPACE_CHARACTER);
        } catch (UnsupportedEncodingException e) {
            // Should not happen - UTF-8 is a valid encoding.
            throw new WalletSaveException("Could not encode string '" + stringToEncode + "'", e);
        }
    }

    /**
     * Decode a string using URL encoding.
     * 
     * @param stringToDecode
     *            The string to URL decode
     */
    public static String decodeURLString(String stringToDecode) {
        try {
            // Earlier multibits did not encode the info file - check if
            // encoded.
            boolean isEncoded = true;
            if (!stringToDecode.contains("%")) {
                // Not encoded.
                isEncoded = false;
            } else {
                // See if there is a % followed by non hex - not encoded.
                int percentPosition = stringToDecode.indexOf('%');
                if (percentPosition > -1) {
                    int nextCharacterPosition = percentPosition + 1;
                    int nextNextCharacterPosition = nextCharacterPosition + 1;
                    if (nextCharacterPosition >= stringToDecode.length() || nextNextCharacterPosition >= stringToDecode.length()) {
                        isEncoded = false;
                    } else {
                        String nextCharacter = stringToDecode.substring(nextCharacterPosition, nextCharacterPosition + 1)
                                .toLowerCase();
                        String nextNextCharacter = stringToDecode.substring(nextNextCharacterPosition,
                                nextNextCharacterPosition + 1).toLowerCase();

                        if (!VALID_HEX_CHARACTERS.contains(nextCharacter) || !VALID_HEX_CHARACTERS.contains(nextNextCharacter)) {
                            isEncoded = false;
                        }
                    }
                }
            }

            if (!isEncoded) {
                return stringToDecode;
            }

            // If there are any spaces convert them to %20s.
            stringToDecode = stringToDecode.replace(ENCODED_SPACE_CHARACTER, "+");

            return java.net.URLDecoder.decode(stringToDecode, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            // Should not happen - UTF-8 is a valid encoding.
            throw new WalletLoadException("Could not decode string '" + stringToDecode + "'", e);
        }
    }

    public boolean isDeleted() {
        return deleted;
    }

    public void setDeleted(boolean deleted) {
        this.deleted = deleted;
    }

    public String getWalletFilename() {
        return walletFilename;
    }

    public MultiBitWalletVersion getWalletVersion() {
        return walletVersion;
    }

    public void setWalletVersion(MultiBitWalletVersion walletVersion) {
        this.walletVersion = walletVersion;
    }
}
