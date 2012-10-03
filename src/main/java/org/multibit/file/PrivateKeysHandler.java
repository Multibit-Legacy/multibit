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
package org.multibit.file;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Scanner;
import java.util.Set;
import java.util.TimeZone;

import org.multibit.crypto.EncrypterDecrypter;
import org.multibit.utils.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;

/**
 * Class for handling reading and writing of private keys to a file
 * 
 * @author jim
 * 
 */
public class PrivateKeysHandler {
    private Logger log = LoggerFactory.getLogger(PrivateKeysHandler.class);

    private static final String COMMENT_STRING_PREFIX = "#";
    private static final int NUMBER_OF_MILLISECONDS_IN_A_SECOND = 1000;

    private SimpleDateFormat formatter;

    private NetworkParameters networkParameters;
    private static final String SEPARATOR = " ";

    private EncrypterDecrypter encrypterDecrypter;

    public PrivateKeysHandler(NetworkParameters networkParameters) {
        // date format is UTC with century, T time separator and Z for UTC
        // timezone
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

        if (networkParameters == null) {
            throw new IllegalArgumentException("NetworkParameters must be supplied");
        }
        this.networkParameters = networkParameters;

        encrypterDecrypter = new EncrypterDecrypter();
    }

    public void exportPrivateKeys(File exportFile, Wallet wallet, BlockChain blockChain, boolean performEncryption, char[] password)
            throws IOException {

        // construct a StringBuffer with the private key export text
        StringBuffer outputStringBuffer = new StringBuffer();

        if (!performEncryption) {
            outputHeaderComment(outputStringBuffer);
        }

        // get the wallet's private keys and output them
        Collection<PrivateKeyAndDate> keyAndDates = createKeyAndDates(wallet, blockChain);
        outputKeys(outputStringBuffer, keyAndDates);

        if (!performEncryption) {
            outputFooterComment(outputStringBuffer);
        }

        String keyOutputText = outputStringBuffer.toString();

        if (performEncryption) {
            EncrypterDecrypter encrypter = new EncrypterDecrypter();
            keyOutputText = encrypter.encrypt(keyOutputText, password);
        }

        FileWriter fileWriter = null;
        PrintWriter printWriter = null;
        try {
            fileWriter = new FileWriter(exportFile);
            printWriter = new PrintWriter(fileWriter);

            printWriter.write(keyOutputText);

            // close the output stream
            printWriter.close();

        } finally {
            if (printWriter != null) {
                printWriter.close();
            }
            if (fileWriter != null) {
                fileWriter.close();
            }
        }
    }

    /**
     * Verify the export file was correctly written to disk
     * 
     * @param exportFile
     *            The export file to verify
     * @param wallet
     *            The wallet to verify the keys against
     * @param performEncryption
     *            Is Encryption required
     * @param password
     *            the password to use is encryption is required
     * @return Verification The result of verification
     */
    public Verification verifyExportFile(File exportFile, Wallet wallet, BlockChain blockChain, boolean performEncryption,
            char[] password) {
        boolean thereWereFailures = false;

        String messageKey = "privateKeysHandler.failedForUnknownReason";
        Object[] messageData = new Object[0];

        try {
            // create the expected export file contents
            Collection<PrivateKeyAndDate> expectedKeysAndDates = createKeyAndDates(wallet, blockChain);

            // read in the specified export file
            Collection<PrivateKeyAndDate> importedKeysAndDates = readInPrivateKeys(exportFile, password);

            if (expectedKeysAndDates.size() != importedKeysAndDates.size()) {
                messageKey = "privateKeysHandler.wrongNumberOfKeys";
                thereWereFailures = true;
            } else {
                for (int i = 0; i < expectedKeysAndDates.size(); i++) {
                    Iterator<PrivateKeyAndDate> iteratorExpected = expectedKeysAndDates.iterator();
                    Iterator<PrivateKeyAndDate> iteratorImported = importedKeysAndDates.iterator();

                    PrivateKeyAndDate expected = iteratorExpected.next();
                    PrivateKeyAndDate imported = iteratorImported.next();

                    if (!Utils.bytesToHexString(expected.getKey().getPrivKeyBytes()).equals(
                            Utils.bytesToHexString(imported.getKey().getPrivKeyBytes()))) {
                        messageKey = "privateKeysHandler.keysDidNotMatch";
                        thereWereFailures = true;
                        break;
                    }

                    // imported keydate must be at or before expected (further
                    // back in time is safe
                    if (imported.getDate().after(expected.getDate())) {
                        messageKey = "privateKeysHandler.keysDidNotMatch";
                        thereWereFailures = true;
                        break;
                    }
                }

            }
        } catch (PrivateKeysHandlerException pkhe) {
            messageKey = "privateKeysHandler.thereWasAnException";
            messageData = new Object[] { pkhe.getMessage() };
            thereWereFailures = true;
        }

        if (!thereWereFailures) {
            messageKey = "privateKeysHandler.verificationSuccess";
        }
        return new Verification(!thereWereFailures, messageKey, messageData);
    }

    public Collection<PrivateKeyAndDate> readInPrivateKeys(File importFile, char[] password) throws PrivateKeysHandlerException {
        if (importFile == null) {
            throw new PrivateKeysHandlerException("Import file cannot be null");
        }

        ArrayList<PrivateKeyAndDate> parseResults = new ArrayList<PrivateKeyAndDate>();

        Scanner scanner = null;

        try {
            // read in the file
            String importFileContents = readFile(importFile);

            if (importFileContents != null && importFileContents.startsWith(encrypterDecrypter.getOpenSSLMagicText())) {
                // decryption required
                EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypter();
                importFileContents = encrypterDecrypter.decrypt(importFileContents, password);
            }

            scanner = new Scanner(new StringReader(importFileContents));

            // first use a Scanner to get each line
            while (scanner.hasNextLine()) {
                processLine(scanner.nextLine(), parseResults);
            }
        } catch (IOException ioe) {
            throw new PrivateKeysHandlerException("Could not read import file '" + importFile.getAbsolutePath() + "'", ioe);
        } finally {
            // ensure the underlying stream is always closed
            // this only has any effect if the item passed
            // to the Scanner
            // constructor implements Closeable (which it
            // does in this case).
            if (scanner != null) {
                scanner.close();
            }
        }
        return parseResults;
    }

    private void outputHeaderComment(StringBuffer out) {
        out.append("# KEEP YOUR PRIVATE KEYS SAFE !").append("\n");
        out.append("# Anyone who can read this file can spend your bitcoin.").append("\n");
        out.append("#").append("\n");
        out.append("# Format:").append("\n");
        out.append("#   <Base58 encoded private key>[<whitespace>[<key createdAt>]]").append("\n");
        out.append("#").append("\n");
        out.append("#   The Base58 encoded private keys are the same format as").append("\n");
        out.append("#   produced by the Satoshi client/ sipa dumpprivkey utility.").append("\n");
        out.append("#").append("\n");
        out.append("#   Key createdAt is in UTC format as specified by ISO 8601").append("\n");
        out.append("#   e.g: 2011-12-31T16:42:00Z . The century, 'T' and 'Z' are mandatory").append("\n");
        out.append("#").append("\n");
    }

    private Collection<PrivateKeyAndDate> createKeyAndDates(Wallet wallet, BlockChain blockChain) {
        Collection<PrivateKeyAndDate> keyAndDates = new ArrayList<PrivateKeyAndDate>();

        if (wallet != null) {
            ArrayList<ECKey> keychain = wallet.keychain;
            Set<Transaction> allTransactions = wallet.getTransactions(true, true);
            if (keychain != null) {
                HashMap<ECKey, Date> keyToEarliestUsageDateMap = new HashMap<ECKey, Date>();

                // the date of the last transaction in the wallet - used where
                // there are no tx for a key
                Date overallLastUsageDate = null;

                for (ECKey ecKey : keychain) {
                    // find the earliest usage of this key
                    Date earliestUsageDate = null;
                    if (allTransactions != null) {
                        for (Transaction tx : allTransactions) {
                            if (transactionUsesKey(tx, ecKey)) {
                                Date updateTime = tx.getUpdateTime();
                                if (updateTime != null) {
                                    if (updateTime != null) {
                                        if (overallLastUsageDate == null) {
                                            overallLastUsageDate = updateTime;
                                        } else {
                                            overallLastUsageDate = overallLastUsageDate.after(updateTime) ? overallLastUsageDate
                                                    : updateTime;
                                        }
                                    }
                                    if (earliestUsageDate == null) {
                                        earliestUsageDate = updateTime;
                                    } else {
                                        earliestUsageDate = earliestUsageDate.before(updateTime) ? earliestUsageDate : updateTime;
                                    }
                                }
                            }
                        }
                    }
                    if (earliestUsageDate != null) {
                        keyToEarliestUsageDateMap.put(ecKey, earliestUsageDate);
                    }
                }

                // if there are no transactions in the wallet
                // overallLastUsageDate will be null.
                // we do not want keys output with a missing date as this forces
                // a replay from
                // the genesis block
                // In this case we know there are no transactions up to the date
                // of the head of the
                // chain so can set the overallLastUsageDate to then
                // On import this will replay from the current chain head to
                // include any future tx.
                if (overallLastUsageDate == null) {
                    if (blockChain != null) {
                        StoredBlock chainHead = blockChain.getChainHead();
                        if (chainHead != null) {
                            Block header = chainHead.getHeader();
                            if (header != null) {
                                long timeSeconds = header.getTimeSeconds();
                                if (timeSeconds != 0) {
                                    overallLastUsageDate = new Date(timeSeconds * NUMBER_OF_MILLISECONDS_IN_A_SECOND);
                                }
                            }
                        }
                    }
                }

                for (ECKey ecKey : keychain) {
                    Date earliestUsageDate = keyToEarliestUsageDateMap.get(ecKey);
                    if (earliestUsageDate == null) {
                        if (overallLastUsageDate != null) {
                            // put the last tx date for the whole wallet in for
                            // this key - there are no tx for this key so this
                            // will be early enough
                            earliestUsageDate = overallLastUsageDate;
                        }
                    }
                    keyAndDates.add(new PrivateKeyAndDate(ecKey, earliestUsageDate));
                }
            }
        }

        return keyAndDates;
    }

    private void outputKeys(StringBuffer out, Collection<PrivateKeyAndDate> keyAndDates) {
        for (PrivateKeyAndDate privateKeyAndDate : keyAndDates) {
            DumpedPrivateKey dumpedPrivateKey = privateKeyAndDate.getKey().getPrivateKeyEncoded(networkParameters);
            String keyOutputString = dumpedPrivateKey.toString();

            if (privateKeyAndDate.getDate() != null) {
                keyOutputString = keyOutputString + SEPARATOR + formatter.format(privateKeyAndDate.getDate());
            }
            out.append(keyOutputString).append("\n");
        }
    }

    private void outputFooterComment(StringBuffer out) {
        out.append("# End of private keys").append("\n");
    }

    public Date calculateReplayDate(Collection<PrivateKeyAndDate> privateKeyAndDates, Wallet wallet) {
        boolean thereWereMissingDates = false;
        Date replayDate =  new Date(DateUtils.nowUtc().getMillis());
        for (PrivateKeyAndDate loop : privateKeyAndDates) {
            if (loop.getDate() == null) {
                thereWereMissingDates = true;
            } else {
                if (loop.getKey() != null) {
                    if (wallet != null && !keyChainContainsPrivateKey(wallet.getKeychain(), loop.getKey())) {
                        replayDate = replayDate.before(loop.getDate()) ? replayDate : loop.getDate();
                    }
                }
            }
        }

        if (thereWereMissingDates) {
            return null;
        } else {
            return replayDate;
        }
    }

    /**
     * this method is here because there is no equals on ECKey
     */
    private boolean keyChainContainsPrivateKey(ArrayList<ECKey> keyChain, ECKey keyToAdd) {
        if (keyChain == null || keyToAdd == null) {
            return false;
        } else {
            for (ECKey loopKey : keyChain) {
                if (Arrays.equals(keyToAdd.getPrivKeyBytes(), loopKey.getPrivKeyBytes())) {
                    return true;
                }
            }
            return false;
        }
    }

    private boolean transactionUsesKey(Transaction transaction, ECKey ecKey) {
        for (TransactionOutput output : transaction.getOutputs()) {
            // This is not thread safe as a key could be removed between the
            // call to isMine and receive.
            try {
                byte[] pubkeyHash = output.getScriptPubKey().getPubKeyHash();
                if (Arrays.equals(ecKey.getPubKeyHash(), pubkeyHash)) {
                    return true;
                }
            } catch (ScriptException e) {
                log.error("Could not parse tx output script: {}", e.toString());
                return false;
            }
        }

        for (TransactionInput input : transaction.getInputs()) {
            // This is not thread safe as a key could be removed between the
            // call to isPubKeyMine and receive.
            try {
                byte[] pubkey = input.getScriptSig().getPubKey();
                if (Arrays.equals(ecKey.getPubKey(), pubkey)) {
                    return true;
                }
            } catch (ScriptException e) {
                log.error("Could not parse tx output script: {}", e.toString());
                return false;
            }
        }
        return false;
    }

    private void processLine(String line, ArrayList<PrivateKeyAndDate> parseResults) {
        if (line != null && !line.trim().equals("") && !line.startsWith(COMMENT_STRING_PREFIX)) {
            Scanner scanner = null;
            try {
                scanner = new Scanner(line);

                String sipaKey = "";
                String createdAtAsString = "";
                if (scanner.hasNext()) {
                    sipaKey = scanner.next();

                }
                if (scanner.hasNext()) {
                    createdAtAsString = scanner.next();
                }

                DumpedPrivateKey dumpedPrivateKey = new DumpedPrivateKey(networkParameters, sipaKey);
                PrivateKeyAndDate privateKeyAndDate = new PrivateKeyAndDate();

                privateKeyAndDate.setKey(dumpedPrivateKey.getKey());

                if (createdAtAsString != null && !"".equals(createdAtAsString)) {
                    Date date = formatter.parse(createdAtAsString);
                    privateKeyAndDate.setDate(date);
                }

                parseResults.add(privateKeyAndDate);
            } catch (AddressFormatException e) {
                throw new PrivateKeysHandlerException("Could not understand address in import file", e);
            } catch (ParseException pe) {
                throw new PrivateKeysHandlerException("Could not parse date in import file", pe);
            } finally {
                if (scanner != null) {
                    scanner.close();
                }
            }
        }
    }

    public static String readFile(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        StringBuilder stringBuilder = new StringBuilder();
        String ls = System.getProperty("line.separator");
        while ((line = reader.readLine()) != null) {
            stringBuilder.append(line);
            stringBuilder.append(ls);
        }
        return stringBuilder.toString();
    }
}
