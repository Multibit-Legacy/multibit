package org.multibit.model;

import java.math.BigInteger;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

import org.multibit.controller.MultiBitController;

import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletEventListener;

/**
 * model containing the MultiBit data
 * 
 * @author jim
 * 
 */
public class MultiBitModel {

    // constants used in the multibit.properties and in data provider data payloads
    
    // MultiBit start up
    public static final String TEST_OR_PRODUCTION_NETWORK = "testOrProductionNetwork";
    public static final String TEST_NETWORK_VALUE = "test";
    public static final String PRODUCTION_NETWORK_VALUE = "production";
    public static final String WALLET_FILENAME = "walletFilename";
   
    // user preferences
    public static final String USER_LANGUAGE_CODE = "languageCode";
    public static final String USER_LANGUAGE_IS_DEFAULT = "isDefault";
    
    // open wallet and save wallet as dialog
    public static final String SELECTED_WALLET_FILENAME = "selectedWalletFilename";

    // send bitcoin and send bitcoin confirm
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";

    
    private Wallet wallet;

    private MultiBitController controller;

    // user preferences
    // note - all the preferences (for all viewsystems are stored in one
    // properties object for simplicity
    private Properties userPreferences;

    private Vector<WalletData> walletData;

    // address book
    private AddressBook addressBook;
    
    public MultiBitModel(MultiBitController controller, Properties userPreferences) {
        this.controller = controller;
        this.userPreferences = userPreferences;

        walletData = new Vector<WalletData>();
        addressBook = new AddressBook();
    }

    /**
     * get a user preference
     * 
     * @param key
     *            String key of property
     * @return String property value
     */
    public String getUserPreference(String key) {
        return userPreferences.getProperty(key);
    }

    /**
     * set a user preference
     * 
     * @return
     */
    public void setUserPreference(String key, String value) {
        userPreferences.put(key, value);
    }

    /**
     * get all user preference
     * 
     * @return
     */
    public Properties getAllUserPreferences() {
        return userPreferences;
    }

    /**
     * set all user preferences
     */
    public void setAllUserPreferences(Properties properties) {
        userPreferences = properties;
    }

    public BigInteger getBalance() {
        if (wallet == null) {
            return new BigInteger("0");
        } else {
            return wallet.getBalance();
        }
    }

    public Vector<WalletData> getWalletData() {
        return walletData;
    }

    public AddressBook getAddressBook() {
        return addressBook;
    }

    public Wallet getWallet() {
        return wallet;
    }

    public void setWallet(Wallet wallet) {
        this.wallet = wallet;
        createWalletData();
    }

    public String getWalletFilename() {
        return userPreferences.getProperty(WALLET_FILENAME);
    }

    public void setWalletFilename(String walletFilename) {
        userPreferences.setProperty(WALLET_FILENAME, walletFilename);
    }

    /**
     * convert the wallet info into walletdata records as they are easier to
     * show to the user
     */
    public Vector<WalletData> createWalletData() {
        Vector<WalletData> walletData = new Vector<WalletData>();

        if (wallet == null) {
            return walletData;
        }
        Collection<Transaction> pendingTransactions = wallet.pendingTransactions();
        Collection<Transaction> unspentTransactions = wallet.unspentTransactions();
        Collection<Transaction> spentTransactions = wallet.spentTransactions();

        if (pendingTransactions != null) {
            for (Transaction pendingTransaction : pendingTransactions) {
                WalletData walletDataRow = new WalletData(pendingTransaction);
                walletData.add(walletDataRow);
                walletDataRow.setCredit(pendingTransaction.getValueSentToMe(wallet));
                try {
                    walletDataRow.setDebit(pendingTransaction.getValueSentFromMe(wallet));
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
                List<TransactionInput> transactionInputs = pendingTransaction.getInputs();
                List<TransactionOutput> transactionOutputs = pendingTransaction.getOutputs();
                if (transactionInputs != null) {
                    TransactionInput firstInput = transactionInputs.get(0);
                    if (firstInput != null) {
                        walletDataRow.setDescription(createDescription(transactionInputs,
                                transactionOutputs, walletDataRow.getCredit(),
                                walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(pendingTransaction));
                walletDataRow.setHeight(workOutHeight(pendingTransaction));
            }
        }

        if (unspentTransactions != null) {
            for (Transaction unspentTransaction : unspentTransactions) {
                WalletData walletDataRow = new WalletData(unspentTransaction);
                walletData.add(walletDataRow);
                walletDataRow.setCredit(unspentTransaction.getValueSentToMe(wallet));
                try {
                    walletDataRow.setDebit(unspentTransaction.getValueSentFromMe(wallet));
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
                List<TransactionInput> transactionInputs = unspentTransaction.getInputs();
                List<TransactionOutput> transactionOutputs = unspentTransaction.getOutputs();
                if (transactionInputs != null) {
                    TransactionInput firstInput = transactionInputs.get(0);
                    if (firstInput != null) {
                        walletDataRow.setDescription(createDescription(transactionInputs,
                                transactionOutputs, walletDataRow.getCredit(),
                                walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(unspentTransaction));
                walletDataRow.setHeight(workOutHeight(unspentTransaction));
            }
        }

        if (spentTransactions != null) {
            for (Transaction spentTransaction : spentTransactions) {
                WalletData walletDataRow = new WalletData(spentTransaction);
                walletData.add(walletDataRow);
                walletDataRow.setCredit(spentTransaction.getValueSentToMe(wallet));
                try {
                    walletDataRow.setDebit(spentTransaction.getValueSentFromMe(wallet));
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
                List<TransactionInput> transactionInputs = spentTransaction.getInputs();
                List<TransactionOutput> transactionOutputs = spentTransaction.getOutputs();

                walletDataRow.setDescription(createDescription(transactionInputs,
                        transactionOutputs, walletDataRow.getCredit(), walletDataRow.getDebit()));

                walletDataRow.setDate(createDate(spentTransaction));
                walletDataRow.setHeight(workOutHeight(spentTransaction));
            }
        }

        return walletData;
    }

    private String createDescription(List<TransactionInput> transactionInputs,
            List<TransactionOutput> transactionOutputs, BigInteger credit, BigInteger debit) {
        String toReturn = "";

        TransactionInput input = null;
        if (transactionInputs != null) {
            input = transactionInputs.get(0);
        }

        TransactionOutput output = null;
        if (transactionOutputs != null) {
            for (TransactionOutput transactionOutput : transactionOutputs) {
                if (transactionOutput != null && !transactionOutput.isMine(wallet)) {
                    output = transactionOutputs.get(0);
                    break;
                }
            }
        }

        if (input != null) {
            if (credit != null && credit.compareTo(BigInteger.ZERO) > 0) {
                // credit
                try {
                    toReturn = controller.getLocaliser().getString(
                            "multiBitModel.creditDescription",
                            new Object[] { input.getFromAddress().toString() });
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
            }
        }
        if (output != null) {
            if (debit != null && debit.compareTo(BigInteger.ZERO) > 0) {
                // debit
                try {
                    toReturn = controller.getLocaliser().getString(
                            "multiBitModel.debitDescription",
                            new Object[] { output.getScriptPubKey().getToAddress() });
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
            }
        }

        return toReturn;
    }

    private Date createDate(Transaction transaction) {
        Set<StoredBlock> appearsIn = transaction.getAppearsIn();
        if (appearsIn != null) {
            if (!appearsIn.isEmpty()) {
                Iterator<StoredBlock> iterator = appearsIn.iterator();
                // just take the first i.e. ignore impact of side chains
                if (iterator.hasNext()) {
                    StoredBlock appearsInStoredBlock = iterator.next();
                    Block appearsInBlock = appearsInStoredBlock.getHeader();
                    // set the time of the block to be the time of the
                    // transaction - TODO get transaction time
                    return new Date(appearsInBlock.getTime() * 1000);
                }
            }
        }
        return null;
    }

    private int workOutHeight(Transaction transaction) {
        Set<StoredBlock> appearsIn = transaction.getAppearsIn();
        if (appearsIn != null) {
            if (!appearsIn.isEmpty()) {
                Iterator<StoredBlock> iterator = appearsIn.iterator();
                // just take the first i.e. ignore impact of side chains
                if (iterator.hasNext()) {
                    StoredBlock appearsInStoredBlock = iterator.next();
                    if (appearsInStoredBlock != null) {
                        return appearsInStoredBlock.getHeight();
                    }
                }
            }
        }
        return -1; // -1 = we do not know
    }

    public void addWalletEventListener(WalletEventListener walletEventListener) {
        wallet.addEventListener(walletEventListener);
    }

    public void removeWalletEventListener(WalletEventListener walletEventListener) {
        wallet.removeEventListener(walletEventListener);
    }
}
