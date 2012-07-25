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
package org.multibit.model;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;

/**
 * Wallet info is the companion info to the bitcoinj that multibit uses it
 * contains the sending and receiving addresses and the wallet version.
 * 
 * It is stored in the same directory as the wallet and has the suffix ".info".
 * 
 * @author jim
 * 
 */
public class WalletInfo {

    private static final Logger log = LoggerFactory.getLogger(WalletInfo.class);
    private static final String ENCODED_SPACE_CHARACTER = "%20";
    private static final String VALID_HEX_CHARACTERS = "01234567890abcdef";

    /**
     * The total receiving addresses known - from the address book (will include
     * keys that are in other wallets).
     */
    private Set<AddressBookData> candidateReceivingAddresses;

    /**
     * The actual receiving addresses exposed for this address book (only keys
     * that occur in this wallet).
     */
    private ArrayList<AddressBookData> receivingAddresses;
    private ArrayList<AddressBookData> sendingAddresses;

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
    private WalletVersion walletVersion;

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
    public WalletInfo(String walletFilename, WalletVersion walletVersion) {
        this.walletFilename = walletFilename;
        this.walletVersion = walletVersion;

        candidateReceivingAddresses = new HashSet<AddressBookData>();
        receivingAddresses = new ArrayList<AddressBookData>();
        sendingAddresses = new ArrayList<AddressBookData>();

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
            
            if (wle.getMessage().indexOf("Cannot understand wallet version") > -1) {
                // throw it again
                throw wle;
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
        return (String) walletPreferences.getProperty(key);
    }

    /**
     * Removes a property from the wallet info.
     * 
     * @param key
     */
    public void remove(String key) {
        walletPreferences.remove(key);
    }
    
    public ArrayList<AddressBookData> getReceivingAddresses() {
        return receivingAddresses;
    }

    public ArrayList<AddressBookData> getSendingAddresses() {
        return sendingAddresses;
    }

    public void setSendingAddresses(ArrayList<AddressBookData> sendingAddresses) {
        this.sendingAddresses = sendingAddresses;
    }

    /**
     * Add a receiving address in the form of an AddressBookData, replacing the
     * label of any existing address.
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
        // check the address is not already in the arraylist
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
     * Write out the wallet info to the file specified as a parameter - a comma
     * separated file format is used.
     * 
     * @param walletInfoFilename
     *            The full path of the wallet info file to write
     * @param walletVersion The wallet version.
     * @throws WalletSaveException
     *             Exception if write is unsuccessful
     */
    public void writeToFile(String walletInfoFilename, WalletVersion walletVersion) throws WalletSaveException {
        BufferedWriter out = null;
        try {
            // We write out the union of the candidate and actual receiving addresses.
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

            // Create file.
            out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(walletInfoFilename), "UTF8"));

            // Write out the multibit addressbook identifier.
            out.write(INFO_MAGIC_TEXT + SEPARATOR + INFO_VERSION_TEXT + "\n");

            // Write out the wallet version.
            out.write(WALLET_VERSION_MARKER + SEPARATOR + walletVersion.getWalletVersionString() + "\n");

            Collection<AddressBookData> receiveAddressValues = allReceivingAddresses.values();
            for (AddressBookData addressBookData : receiveAddressValues) {
                String columnOne = RECEIVE_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + encodeURLString(columnThree) + "\n");
            }

            for (AddressBookData addressBookData : sendingAddresses) {
                String columnOne = SEND_ADDRESS_MARKER;
                String columnTwo = addressBookData.getAddress();
                String columnThree = addressBookData.getLabel();
                if (columnTwo == null) {
                    columnTwo = "";
                }
                out.write(columnOne + SEPARATOR + columnTwo + SEPARATOR + encodeURLString(columnThree) + "\n");
            }

            for (Object key : walletPreferences.keySet()) {
                String columnOne = PROPERTY_MARKER;
                String columnTwo = (String) key;
                String encodedColumnThree = encodeURLString((String) walletPreferences.get(key));
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
                throw new WalletLoadException("The file '" + walletInfoFilename + "' is not a valid wallet info file (empty line 1)");
            }
            StringTokenizer tokenizer = new StringTokenizer(firstLine, SEPARATOR);
            int numberOfTokens = tokenizer.countTokens();
            if (numberOfTokens == 2) {
                String magicText = tokenizer.nextToken();
                String versionNumber = tokenizer.nextToken();
                if (!INFO_MAGIC_TEXT.equals(magicText) || !INFO_VERSION_TEXT.equals(versionNumber)) {
                    // This is not an multibit address book.
                    throw new WalletLoadException("The file '" + walletInfoFilename + "' is not a valid wallet info file (wrong magic number on line 1)");
                }
            } else {
                // This is not an multibit address book.
                throw new WalletLoadException("The file '" + walletInfoFilename + "' is not a valid wallet info file (wrong number of tokens on line 1)");
            }

            // Read the wallet version.
            String secondLine = bufferedReader.readLine();
            StringTokenizer walletVersionTokenizer = new StringTokenizer(secondLine, SEPARATOR);
            int walletVersionTokenNumber = walletVersionTokenizer.countTokens();
            if (walletVersionTokenNumber == 2) {
                String walletVersionMarker = walletVersionTokenizer.nextToken();
                String walletVersionString = walletVersionTokenizer.nextToken();
                if (!WALLET_VERSION_MARKER.equals(walletVersionMarker) 
                                || !(WalletVersion.SERIALIZED.getWalletVersionString().equals(walletVersionString) 
                                || WalletVersion.PROTOBUF.getWalletVersionString().equals(walletVersionString))) {
                    // This refers to a version of the wallet we do not know about.
                    throw new WalletLoadException("Cannot understand wallet version of '" + walletVersionMarker + "', '" + walletVersionString + "'" );
                } else {
                    // The wallet version passed in the file is used rather than the value in the constructor
                    if (!walletVersion.getWalletVersionString().equals(walletVersionString)) {
                        log.debug("The wallet version in the constructor was '" + walletVersion + "'. In the wallet info file it was '" + walletVersionString + "'. Using the latter.");
                        if (WalletVersion.SERIALIZED.getWalletVersionString().equals(walletVersionString)) {
                            walletVersion = WalletVersion.SERIALIZED;
                        } else if (WalletVersion.PROTOBUF.getWalletVersionString().equals(walletVersionString)) {
                            walletVersion = WalletVersion.PROTOBUF;
                        } 
                    }                
                }
            } else {
                // The format of the info format is wrong.
                throw new WalletLoadException("Cannot understand wallet version text of '" + secondLine + "'");
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
                            addReceivingAddress(new AddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo), false);
                        } else {
                            if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                                addSendingAddress(new AddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo));
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
                        addReceivingAddress(new AddressBookData(decodedColumnThreeValue, columnTwo), false);
                    } else {
                        if (SEND_ADDRESS_MARKER.equals(columnOne)) {
                            addSendingAddress(new AddressBookData(decodedColumnThreeValue, columnTwo));
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
                    // This is a multiline column 3 (typically a multiline label).
                    isMultilineColumnThree = true;
                    multilineColumnThreeValue = multilineColumnThreeValue + "\n" + inputLine;
                }
            }
            if (isMultilineColumnThree) {
                // Add previous multiline column three to model.
                String decodedMultiLineColumnThreeValue = decodeURLString(multilineColumnThreeValue);
                if (RECEIVE_ADDRESS_MARKER.equals(previousColumnOne)) {
                    addReceivingAddress(new AddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo), false);
                } else {
                    if (SEND_ADDRESS_MARKER.equals(previousColumnOne)) {
                        addSendingAddress(new AddressBookData(decodedMultiLineColumnThreeValue, previousColumnTwo));
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
        if (walletFilename.endsWith(MultiBitModel.WALLET_FILE_EXTENSION)) {
            walletInfoFilename = walletInfoFilename.substring(0,
                    walletFilename.length() - MultiBitModel.WALLET_FILE_EXTENSION.length() - 1);
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
            throw new  WalletSaveException("Could not encode string '" + stringToEncode + "'", e);
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
            // Earlier multibits did not encode the info file - check if encoded.
            boolean isEncoded = true;
            if (!stringToDecode.contains("%")) {
                // Not encoded.
                isEncoded = false;
            } else {
                // See if there is a % followed by non hex - not encoded.
                int percentPosition = stringToDecode.indexOf("%");
                if (percentPosition > -1 ) {
                    int nextCharacterPosition = percentPosition + 1;
                    int nextNextCharacterPosition = nextCharacterPosition + 1;
                    if (nextCharacterPosition >= stringToDecode.length() || nextNextCharacterPosition >= stringToDecode.length()) {
                        isEncoded = false;
                    } else {
                        String nextCharacter = stringToDecode.substring(nextCharacterPosition, nextCharacterPosition + 1).toLowerCase();
                        String nextNextCharacter = stringToDecode.substring(nextNextCharacterPosition, nextNextCharacterPosition + 1).toLowerCase();
                        
                        if (VALID_HEX_CHARACTERS.indexOf(nextCharacter) < 0 || VALID_HEX_CHARACTERS.indexOf(nextNextCharacter) < 0) {
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
            
            String decodedText = java.net.URLDecoder.decode(stringToDecode, "UTF-8");
            return decodedText;
        } catch (UnsupportedEncodingException e) {
            // Should not happen - UTF-8 is a valid encoding.
            throw new  WalletLoadException("Could not decode string '" + stringToDecode + "'", e);
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

    public WalletVersion getWalletVersion() {
        return walletVersion;
    }

    public void setWalletVersion(WalletVersion walletVersion) {
        this.walletVersion = walletVersion;
    }
}
