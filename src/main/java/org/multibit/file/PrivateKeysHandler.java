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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Scanner;
import java.util.Set;
import java.util.TimeZone;

import org.multibit.crypto.EncrypterDecrypter;
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

    public PrivateKeysHandler(NetworkParameters networkParameters) {
        // date format is UTC with century, T time separator and Z for UTC
        // timezone
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

        if (networkParameters == null) {
            throw new IllegalArgumentException("NetworkParameters must be supplied");
        }
        this.networkParameters = networkParameters;
    }

    public void exportPrivateKeys(File exportFile, Wallet wallet, BlockChain blockChain, boolean performEncryption, char[] password) throws IOException {
        
        //  construct a StringBuffer with the private key export text
        StringBuffer outputStringBuffer = new StringBuffer();
    
        outputHeaderComment(outputStringBuffer);

        // get the wallet's private keys and output them
        outputKeys(outputStringBuffer, wallet, blockChain);

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
     * @param exportFile The export file to verify
     * @param wallet The wallet to verify the keys against
     * @param performEncryption Is Encryption required
     * @param password the password to use is encryption is required
     * @return A localised string suitable for user consumption
     */
    public String verifyExportFile(File exportFile, Wallet wallet, boolean performEncryption, char[] password) {
        return "File verification not implemented.";
    }

    public ArrayList<PrivateKeyAndDate> importPrivateKeys(File importFile) throws PrivateKeysHandlerException {
        if (importFile == null) {
            throw new PrivateKeysHandlerException("Import file cannot be null");
        }

        ArrayList<PrivateKeyAndDate> parseResults = new ArrayList<PrivateKeyAndDate>();

        Scanner scanner = null;
        try {
            scanner = new Scanner(new FileReader(importFile));

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

    private void outputKeys(StringBuffer out, Wallet wallet, BlockChain blockChain) {
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
                                Date updateTime = null;
                                Date updateTime1 = tx.getUpdateTime();
                                Date updateTime2 = tx.getUpdatedAt();
                                if (updateTime1 == null) {
                                    if (updateTime2 == null) {
                                        // no data
                                    } else {
                                        updateTime = updateTime2;
                                    }
                                } else {
                                    if (updateTime2 == null) {
                                        updateTime = updateTime1;
                                    } else {
                                        updateTime = updateTime1.before(updateTime2) ? updateTime1 : updateTime2;
                                    }
                                }
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
                    DumpedPrivateKey dumpedPrivateKey = ecKey.getPrivateKeyEncoded(networkParameters);
                    String keyOutputString = dumpedPrivateKey.toString();
                    Date earliestUsageDate = keyToEarliestUsageDateMap.get(ecKey);
                    if (earliestUsageDate == null) {
                        if (overallLastUsageDate != null) {
                            // put the last tx date for the whole wallet in for
                            // this key - there are no tx for this key so this
                            // will be early enough
                            earliestUsageDate = overallLastUsageDate;
                        }
                    }
                    if (earliestUsageDate != null) {
                        keyOutputString = keyOutputString + SEPARATOR + formatter.format(earliestUsageDate);
                    }
                    out.append(keyOutputString).append("\n");
                }
            }
        }
        out.append("# End of private keys").append("\n");;
    }

    private boolean transactionUsesKey(Transaction transaction, ECKey ecKey) {
        try {
            for (TransactionOutput output : transaction.getOutputs()) {
                // TODO: Handle more types of outputs, not just regular to
                // address outputs.
                if (output.getScriptPubKey().isSentToIP())
                    continue;
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
                if (input.getScriptSig().isSentToIP())
                    continue;

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
        } catch (ScriptException e) {
            throw new PrivateKeysHandlerException("Transaction script was not understandable", e);
        }
    }

    private void processLine(String line, ArrayList<PrivateKeyAndDate> parseResults) {
        if (line == null || line.trim().equals("") || line.startsWith(COMMENT_STRING_PREFIX)) {
            // nothing to do
        } else {
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
}
