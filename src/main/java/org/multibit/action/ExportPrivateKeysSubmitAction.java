package org.multibit.action;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Export Private Keys submit action
 * 
 * @author jim
 * 
 */
public class ExportPrivateKeysSubmitAction implements Action {
    private static final Logger log = LoggerFactory.getLogger(ExportPrivateKeysSubmitAction.class);

    private MultiBitController controller;

    private static final String SEPARATOR = " ";

    private SimpleDateFormat formatter;

    public ExportPrivateKeysSubmitAction(MultiBitController controller) {
        this.controller = controller;
        
        // date format is UTC with century, T time separator and Z for UTC timezone
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    }

    public void execute(DataProvider dataProvider) { 
         String message = controller.getLocaliser().getString("showExportPrivateKeysAction.noDataWasWritten");
        
        // get the required output file
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item privateKeysOutputFilenameItem = data.getItem(MultiBitModel.PRIVATE_KEY_FILENAME);
                if (privateKeysOutputFilenameItem != null) {
                    String outputFilename = (String) privateKeysOutputFilenameItem.getNewValue();
                    // TODO check to see if file already exists - do not
                    // overwrite

                    try {
                        // Create file
                        FileWriter fstream = new FileWriter(outputFilename);
                        PrintWriter out = new PrintWriter(fstream);

                        outputHeaderComment(out);

                        // get the wallet's private keys and output them
                        Wallet activeWallet = controller.getModel().getActivePerWalletModelData().getWallet();
                        outputKeys(out, activeWallet);

                        // Close the output stream
                        out.close();
                        
                        message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportSuccess");
                    } catch (Exception e) {// Catch exception if any
                        log.error(e.getClass().getName() + " " + e.getMessage());
                        message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportFailure", new Object[] {e.getClass().getName() + " " + e.getMessage()});
                    }
                }
            }
        }
        
        controller.getModel().setUserPreference(MultiBitModel.DISPLAY_EXPORT_PRIVATE_KEYS_MESSAGE, "true");
        controller.getModel().setUserPreference(MultiBitModel.EXPORT_PRIVATE_KEYS_MESSAGE, message);
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }

    private void outputHeaderComment(PrintWriter out) throws IOException {
        out.println("# KEEP YOUR PRIVATE KEYS SAFE !");
        out.println("# Anyone who can read this file can spend your bitcoin.");
        out.println("#");
        out.println("# Format:");
        out.println("#   <Base58 encoded private key>[<whitespace>[<key createdAt>]]");
        out.println("#");
        out.println("#   The Base58 encoded private keys are the same format as");
        out.println("#   produced by the Satoshi client/ sipa dumpprivkey utility.");
        out.println("#");
        out.println("#   Key createdAt is in UTC format as specified by ISO 8601");
        out.println("#   e.g: 2011-12-31T16:42:00Z . The century, 'T' and 'Z' are mandatory");
        out.println("#");
    }

    private void outputKeys(PrintWriter out, Wallet wallet) throws IOException {
        if (wallet != null) {
            ArrayList<ECKey> keychain = wallet.keychain;
            Set<Transaction> allTransactions = wallet.getTransactions(true, true);
            if (keychain != null) {
                HashMap<ECKey, Date> keyToEarliestUsageDateMap = new HashMap<ECKey, Date>();
                
                // the date of the last transaction in the wallet - used where there are no tx for a key
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
                                            overallLastUsageDate = overallLastUsageDate.after(updateTime) ? overallLastUsageDate : updateTime;
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
                
                for (ECKey ecKey : keychain) {
                    DumpedPrivateKey dumpedPrivateKey = ecKey.getPrivateKeyEncoded(controller.getMultiBitService()
                            .getNetworkParameters());
                    String keyOutputString = dumpedPrivateKey.toString();
                    Date earliestUsageDate = keyToEarliestUsageDateMap.get(ecKey);
                    if (earliestUsageDate == null) {
                        if (overallLastUsageDate != null) {
                            // put the last tx date for the whole wallet in for this key - there are no tx for this key so this will be early enough
                            earliestUsageDate = overallLastUsageDate;
                        }
                    }
                    if (earliestUsageDate != null) {
                        keyOutputString = keyOutputString + SEPARATOR + formatter.format(earliestUsageDate);
                    }
                    out.println(keyOutputString);
                }
            }
        }
        out.println("# End of private keys");
    }
    
    private boolean transactionUsesKey(Transaction transaction, ECKey ecKey) {
        try {
            for (TransactionOutput output : transaction.getOutputs()) {
                // TODO: Handle more types of outputs, not just regular to address outputs.
                if (output.getScriptPubKey().isSentToIP()) continue;
                // This is not thread safe as a key could be removed between the call to isMine and receive.
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
                if (input.getScriptSig().isSentToIP()) continue;
                
                // This is not thread safe as a key could be removed between the call to isPubKeyMine and receive.
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
            throw new RuntimeException(e);
        }        
    }
}
