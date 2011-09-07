package org.multibit.model;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

import org.multibit.controller.MultiBitController;
import org.multibit.network.MultiBitService;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.Wallet.BalanceType;
import com.google.bitcoin.core.WalletEventListener;

/**
 * model containing the MultiBit data
 * 
 * @author jim
 * 
 */
public class MultiBitModel {

    // constants used in the multibit.properties and in data provider data
    // payloads

    // MultiBit start up
    public static final String TEST_OR_PRODUCTION_NETWORK = "testOrProductionNetwork";
    public static final String TEST_NETWORK_VALUE = "test";
    public static final String PRODUCTION_NETWORK_VALUE = "production";
    public static final String WALLET_FILENAME = "walletFilename";

    // user preferences
    public static final String SELECTED_VIEW = "selectedView";
    public static final String USER_LANGUAGE_CODE = "languageCode";
    public static final String USER_LANGUAGE_IS_DEFAULT = "isDefault";

    // open wallet and save wallet as dialog
    public static final String SELECTED_WALLET_FILENAME = "selectedWalletFilename";

    // send bitcoin and send bitcoin confirm
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";
    public static final String SEND_FEE = "sendFee";
    public static final BigInteger SEND_FEE_DEFAULT = new BigInteger("100000"); // =
                                                                                // 0.001
                                                                                // BTC
                                                                                // =
                                                                                // twice
                                                                                // the
                                                                                // minimum
                                                                                // fee
                                                                                // as
                                                                                // of
                                                                                // 1
                                                                                // sep
                                                                                // 2011

    // receive bitcoin
    public static final String RECEIVE_ADDRESS = "receiveAddress";
    public static final String RECEIVE_LABEL = "receiveLabel";
    public static final String RECEIVE_AMOUNT = "receiveAmount";
    public static final String RECEIVE_NEW_KEY = "receiveNewKey"; // to delete
    public static final String RECEIVE_URI_TEXT = "receiveUriText";
    public static final String RECEIVE_URI_IMAGE = "receiveUriImage";

    // validation
    public static final String VALIDATION_ADDRESS_IS_INVALID = "validationAddressIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_INVALID = "validationAmountIsInvalid";
    public static final String VALIDATION_NOT_ENOUGH_FUNDS = "validationNotEnoughFunds";
    public static final String VALIDATION_NOT_ENOUGH_FUNDS_BUT_ESTIMATED_OK = "validationNotEnoughFundsButEstimatedOk";
    public static final String VALIDATION_ADDRESS_VALUE = "validationAddressValue";
    public static final String VALIDATION_AMOUNT_VALUE = "validationAmountValue";

    private Wallet wallet;

    private final MultiBitController controller;

    // user preferences
    // note - all the preferences (for all viewsystems are stored in one
    // properties object for simplicity
    private Properties userPreferences;

    private Vector<WalletTableData> walletData;

    // wallet info including address labelling
    private WalletInfo walletInfo;

    public MultiBitModel(MultiBitController controller, Properties userPreferences) {
        this.controller = controller;
        this.userPreferences = userPreferences;

        walletData = new Vector<WalletTableData>();
        walletInfo = null;
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

    /**
     * get a wallet preference
     * 
     * @param key
     *            String key of property
     * @return String property value
     */
    public String getWalletPreference(String key) {
        if (walletInfo != null) {
            return walletInfo.getProperty(key);
        } else {
            return null;
        }
    }

    /**
     * set a wallet preference
     * 
     * @return
     */
    public void setWalletPreference(String key, String value) {
        if (walletInfo != null) {
            walletInfo.put(key, value);
        }
    }

    public BigInteger getBalance() {
        if (wallet == null) {
            return new BigInteger("0");
        } else {
            return wallet.getBalance(BalanceType.ESTIMATED);
        }
    }

    public Vector<WalletTableData> getWalletData() {
        return walletData;
    }

    public WalletInfo getWalletInfo() {
        return walletInfo;
    }

    public Wallet getWallet() {
        return wallet;
    }

    public void setWallet(Wallet wallet) {
        this.wallet = wallet;

        // wire up the controller as a wallet event listener
        wallet.addEventListener(new WalletEventListener() {
            public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
                controller.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
            }

            public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance,
                    BigInteger newBalance) {
                controller.onPendingCoinsReceived(wallet, transaction, prevBalance, newBalance);
            }

            public void onPendingCoinsReceived(Wallet wallet, Transaction transaction) {
                controller.onPendingCoinsReceived(wallet, transaction, null, null);
            }

        });

        createWalletData();
        createAddressBookReceivingAddresses();
    }

    public String getWalletFilename() {
        return userPreferences.getProperty(WALLET_FILENAME);
    }

    public void setWalletFilename(String walletFilename) {
        userPreferences.setProperty(WALLET_FILENAME, walletFilename);
    }

    /**
     * convert the wallet info into walletdata records as they are easier to
     * show to the user in tabular form
     */
    public Vector<WalletTableData> createWalletData() {
        Vector<WalletTableData> walletData = new Vector<WalletTableData>();

        if (wallet == null) {
            return walletData;
        }
        Collection<Transaction> pendingTransactions = wallet.pendingTransactions();
        Collection<Transaction> unspentTransactions = wallet.unspentTransactions();
        Collection<Transaction> spentTransactions = wallet.spentTransactions();

        if (pendingTransactions != null) {
            for (Transaction pendingTransaction : pendingTransactions) {
                WalletTableData walletDataRow = new WalletTableData(pendingTransaction);
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
                        walletDataRow.setDescription(createDescription(transactionInputs, transactionOutputs,
                                walletDataRow.getCredit(), walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(pendingTransaction));
                walletDataRow.setHeight(workOutHeight(pendingTransaction));
            }
        }

        if (unspentTransactions != null) {
            for (Transaction unspentTransaction : unspentTransactions) {
                WalletTableData walletDataRow = new WalletTableData(unspentTransaction);
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
                        walletDataRow.setDescription(createDescription(transactionInputs, transactionOutputs,
                                walletDataRow.getCredit(), walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(unspentTransaction));
                walletDataRow.setHeight(workOutHeight(unspentTransaction));
            }
        }

        if (spentTransactions != null) {
            for (Transaction spentTransaction : spentTransactions) {
                WalletTableData walletDataRow = new WalletTableData(spentTransaction);
                walletData.add(walletDataRow);
                walletDataRow.setCredit(spentTransaction.getValueSentToMe(wallet));
                try {
                    walletDataRow.setDebit(spentTransaction.getValueSentFromMe(wallet));
                } catch (ScriptException e) {
                    e.printStackTrace();
                }
                List<TransactionInput> transactionInputs = spentTransaction.getInputs();
                List<TransactionOutput> transactionOutputs = spentTransaction.getOutputs();

                walletDataRow.setDescription(createDescription(transactionInputs, transactionOutputs,
                        walletDataRow.getCredit(), walletDataRow.getDebit()));

                walletDataRow.setDate(createDate(spentTransaction));
                walletDataRow.setHeight(workOutHeight(spentTransaction));
            }
        }

        // run through all the walletdata to see if both credit and debit are
        // set (this means change was received)
        for (WalletTableData walletDataRow : walletData) {
            if (walletDataRow.getCredit() != null && (walletDataRow.getCredit().compareTo(BigInteger.ZERO) > 0)
                    && (walletDataRow.getDebit() != null) && walletDataRow.getDebit().compareTo(BigInteger.ZERO) > 0) {
                BigInteger net = walletDataRow.getCredit().subtract(walletDataRow.getDebit());
                if (net.compareTo(BigInteger.ZERO) >= 0) {
                    walletDataRow.setCredit(net);
                    walletDataRow.setDebit(BigInteger.ZERO);
                } else {
                    walletDataRow.setCredit(BigInteger.ZERO);
                    walletDataRow.setDebit(net.negate());
                }
            }
        }

        return walletData;
    }

    /**
     * add the receiving addresses of all the keys in this wallet
     */
    public void createAddressBookReceivingAddresses() {
        if (wallet != null) {
            ArrayList<ECKey> keyChain = wallet.keychain;
            if (keyChain != null) {
                MultiBitService multiBitService = controller.getMultiBitService();
                if (multiBitService != null) {
                    NetworkParameters networkParameters = multiBitService.getNetworkParameters();
                    if (networkParameters != null) {
                        if (walletInfo != null) {
                            // clear the existing receiving addresses
                            walletInfo.getReceivingAddresses().clear();
                            for (ECKey key : keyChain) {
                                Address address = key.toAddress(controller.getMultiBitService().getNetworkParameters());
                                walletInfo.addReceivingAddressOfKey(address);
                            }
                        }
                    }
                }
            }
        }
    }

    public String createDescription(List<TransactionInput> transactionInputs, List<TransactionOutput> transactionOutputs,
            BigInteger credit, BigInteger debit) {
        String toReturn = "";

        TransactionInput input = null;
        if (transactionInputs != null) {
            input = transactionInputs.get(0);
        }

        TransactionOutput myOutput = null;
        TransactionOutput theirOutput = null;
        if (transactionOutputs != null) {
            for (TransactionOutput transactionOutput : transactionOutputs) {
                if (transactionOutput != null && transactionOutput.isMine(wallet)) {
                    myOutput = transactionOutput;
                }
                if (transactionOutput != null && !transactionOutput.isMine(wallet)) {
                    theirOutput = transactionOutput;
                }
            }
        }

        if (credit != null && credit.compareTo(BigInteger.ZERO) > 0) {
            // credit
            try {
                String addressString = "";

                if (controller.getMultiBitService() != null && myOutput != null) {
                    Address toAddress = new Address(controller.getMultiBitService().getNetworkParameters(), myOutput
                            .getScriptPubKey().getPubKeyHash());
                    addressString = toAddress.toString();
                }

                // String addressString = input.getFromAddress().toString();
                String label = null;
                if (walletInfo != null) {
                    label = walletInfo.lookupLabelForReceivingAddress(addressString);
                }
                if (label != null && label != "") {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescriptionWithLabel",
                            new Object[] { addressString, label });
                } else {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescription",
                            new Object[] { addressString });
                }
            } catch (ScriptException e) {
                e.printStackTrace();
            }
        }

        if (debit != null && debit.compareTo(BigInteger.ZERO) > 0) {
            // debit
            try {
                // see if the address is a known sending address
                if (theirOutput != null) {
                    String addressString = theirOutput.getScriptPubKey().getToAddress().toString();
                    String label = null;
                    if (walletInfo != null) {
                        label = walletInfo.lookupLabelForSendingAddress(addressString);
                    }
                    if (label != null && label != "") {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescriptionWithLabel",
                                new Object[] { addressString, label });
                    } else {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescription",
                                new Object[] { addressString });
                    }
                }
            } catch (ScriptException e) {
                e.printStackTrace();
            }
        }

        return toReturn;
    }

    private Date createDate(Transaction transaction) {
        // if transaction has altered date - return that
        if (transaction.getUpdatedAt() != null) {
            return transaction.getUpdatedAt();
        }

        // other wise return the date of the block it first appeared in
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
                    return new Date(appearsInBlock.getTimeSeconds() * 1000);
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

    public void setWalletInfo(WalletInfo walletInfo) {
        this.walletInfo = walletInfo;
    }
}
