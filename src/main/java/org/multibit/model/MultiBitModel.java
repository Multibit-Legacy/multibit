package org.multibit.model;

import com.google.bitcoin.core.*;
import com.google.bitcoin.core.Wallet.BalanceType;
import org.multibit.controller.MultiBitController;
import org.multibit.network.MultiBitService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.*;

/**
 * model containing the MultiBit data
 * 
 * most of the methods act on the single, active wallet in the model
 * 
 * @author jim
 * 
 */
public class MultiBitModel {

    private static final Logger log = LoggerFactory.getLogger(MultiBitModel.class);

    // constants used in the multibit.properties and in data provider data
    // payloads

    // MultiBit start up
    public static final String TEST_OR_PRODUCTION_NETWORK = "testOrProductionNetwork";
    public static final String TEST_NETWORK_VALUE = "test";
    public static final String PRODUCTION_NETWORK_VALUE = "production";
    public static final String WALLET_FILENAME = "walletFilename";

    // user preferences
    public static final String SELECTED_VIEW = "selectedView";
    public static final String PREVIOUSLY_SELECTED_VIEW = "previousView";

    public static final String USER_LANGUAGE_CODE = "languageCode";
    public static final String USER_LANGUAGE_IS_DEFAULT = "isDefault";

    // my wallets, open wallet and save wallet as dialog
    public static final String ACTIVE_WALLET_FILENAME = "selectedWalletFilename";
    public static final String WALLET_FILENAME_PREFIX = "walletFilename.";
    public static final String WALLET_DESCRIPTION_PREFIX = "walletDescription.";
    public static final String NUMBER_OF_WALLETS = "numberOfWallets";

    // send bitcoin and send bitcoin confirm
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";
    public static final String SEND_URI_IMAGE = "sendUriImage";

    public static final String SEND_FEE = "sendFee";
    // default is min fee of 0.0005 BTC
    public static final BigInteger SEND_FEE_DEFAULT = new BigInteger("10000");
    // min fee of 0.0001 BTC
    public static final BigInteger SEND_MINIMUM_FEE = new BigInteger("10000");

    public static final String SEND_WAS_SUCCESSFUL = "sendWasSuccessful";
    public static final String SEND_ERROR_MESSAGE = "sendErrorMessage";

    // receive bitcoin
    public static final String IS_RECEIVE_BITCOIN = "isReceiveBitcoin";
    public static final String RECEIVE_ADDRESS = "receiveAddress";
    public static final String RECEIVE_LABEL = "receiveLabel";
    public static final String RECEIVE_AMOUNT = "receiveAmount";
    public static final String RECEIVE_NEW_KEY = "receiveNewKey"; // to delete
    public static final String RECEIVE_URI_IMAGE = "receiveUriImage";

    // validation
    public static final String VALIDATION_ADDRESS_IS_INVALID = "validationAddressIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_INVALID = "validationAmountIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_MISSING = "validationAmountIsMissing";
    public static final String VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO = "validationAmountIsNegativeOrZero";
    public static final String VALIDATION_NOT_ENOUGH_FUNDS = "validationNotEnoughFunds";
    public static final String VALIDATION_ADDRESS_VALUE = "validationAddressValue";
    public static final String VALIDATION_AMOUNT_VALUE = "validationAmountValue";

    public static final String WALLET_FILE_EXTENSION = "wallet";

    // connect to single node
    public static final String SINGLE_NODE_CONNECTION = "singleNodeConnection";
    
    // sizes and last modified dates of files
    public static final String WALLET_FILE_SIZE = "walletFileSize";
    public static final String WALLET_FILE_LAST_MODIFIED = "walletFileLastModified";

    public static final String WALLET_INFO_FILE_SIZE = "walletInfoFileSize";
    public static final String WALLET_INFO_FILE_LAST_MODIFIED = "walletInfoFileLastModified";

    private final MultiBitController controller;

    // user preferences
    private Properties userPreferences;

    /**
     * list of each wallet's total model data
     */
    private List<PerWalletModelData> perWalletModelDataList;

    /**
     * the current active wallet
     */
    private PerWalletModelData activeWalletModelData;

    public MultiBitModel(MultiBitController controller) {
        this(controller, new Properties());
    }

    public MultiBitModel(MultiBitController controller, Properties userPreferences) {
        this.controller = controller;
        this.userPreferences = userPreferences;

        perWalletModelDataList = new LinkedList<PerWalletModelData>();

        activeWalletModelData = new PerWalletModelData();
        perWalletModelDataList.add(activeWalletModelData);
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
        if (key != null && value != null) {
            userPreferences.put(key, value);
        }
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

    public PerWalletModelData getActivePerWalletModelData() {
        return activeWalletModelData;
    }
    
    /**
     * get a wallet preference from the active wallet
     * 
     * @param key
     *            String key of property
     * @return String property value
     */
    public String getActiveWalletPreference(String key) {
        if (activeWalletModelData.getWalletInfo() != null) {
            return activeWalletModelData.getWalletInfo().getProperty(key);
        } else {
            return null;
        }
    }

    /**
     * set a wallet preference from the active wallet
     * 
     * @return
     */
    public void setActiveWalletPreference(String key, String value) {
        if (activeWalletModelData.getWalletInfo() != null && value != null) {
            activeWalletModelData.getWalletInfo().put(key, value);
            activeWalletModelData.setDirty(true);            
        }
    }

    /**
     * get the estimated balance of the active wallet
     * 
     * @return
     */
    public BigInteger getActiveWalletEstimatedBalance() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED);
        }
    }

    /**
     * get the available balance of the active wallet
     * 
     * @return
     */
    public BigInteger getActiveWalletAvailableBalance() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(BalanceType.AVAILABLE);
        }
    }

    /**
     * get the wallet data for the active wallet
     * 
     * @return
     */
    public List<WalletTableData> getActiveWalletWalletData() {
        return activeWalletModelData.getWalletTableDataList();
    }

    /**
     * get the wallet info for the active wallet
     * 
     * @return
     */
    public WalletInfo getActiveWalletWalletInfo() {
        return activeWalletModelData.getWalletInfo();
    }

    /**
     * get the active wallet
     * 
     * @return
     */
    public Wallet getActiveWallet() {
        return activeWalletModelData.getWallet();
    }

    /**
     * set the active wallet, given a wallet filename
     * 
     * @param wallet
     */
    public void setActiveWalletByFilename(String walletFilename) {
        if (walletFilename == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    activeWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }
    }
    
    /**
     * set a wallet description, given a wallet filename
     * 
     * @param wallet
     */
    public void setWalletDescriptionByFilename(String walletFilename, String walletDescription) {
        if (walletFilename == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    loopPerWalletModelData.setWalletDescription(walletDescription);
                    loopPerWalletModelData.setDirty(true);
                    break;
                }
            }
        }
    }
    
    /**
     * add a new wallet to the list of managed wallets
     */
    public PerWalletModelData addWallet(Wallet wallet, String walletFilename) {
        if (walletFilename == null) {
            return null;
        }

        // check to see if it is already in the managed list - no need to add it
        // again if so
        for (PerWalletModelData loopModelData : perWalletModelDataList) {
            if (walletFilename.equals(loopModelData.getWalletFilename())) {
                return loopModelData;
            }
        }

        PerWalletModelData newPerWalletModelData = new PerWalletModelData();
        newPerWalletModelData.setWallet(wallet);
        newPerWalletModelData.setWalletFilename(walletFilename);

        // table row data used in displaying transactions - initially empty
        newPerWalletModelData.setWalletTableDataList(new ArrayList<WalletTableData>());

        // if it is the initial empty activeWalletModelData remove it
        if (activeWalletModelData != null
                && ("".equals(activeWalletModelData.getWalletFilename()) || activeWalletModelData.getWalletFilename() == null)) {
            perWalletModelDataList.remove(activeWalletModelData);
            activeWalletModelData = newPerWalletModelData;
        }

        perWalletModelDataList.add(newPerWalletModelData);

        // wire up the controller as a wallet event listener
        if (wallet != null) {
            wallet.addEventListener(new WalletEventListener() {
                public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance,
                        BigInteger newBalance) {
                    controller.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
                }

                public void onPendingCoinsReceived(Wallet wallet, Transaction transaction) {
                    controller.onPendingCoinsReceived(wallet, transaction);
                }

                @Override
                public void onReorganize(Wallet wallet) {
                }

                @Override
                public void onDeadTransaction(Wallet wallet, Transaction deadTx, Transaction replacementTx) {
                }

            });
        }

        createWalletData(walletFilename);
        createAddressBookReceivingAddresses(walletFilename);
        
        return newPerWalletModelData;
    }

    /**
     * get the active wallet filename
     * 
     * 
     * @return
     */
    public String getActiveWalletFilename() {
        return activeWalletModelData.getWalletFilename();
    }

    /**
     * convert the active wallet info into walletdata records as they are easier
     * to show to the user in tabular form
     */
    public Vector<WalletTableData> createWalletData(String walletFilename) {
        // TODO Consider an ArrayList if possible
        Vector<WalletTableData> walletData = new Vector<WalletTableData>();

        if (walletFilename == null) {
            return walletData;
        }
        
        PerWalletModelData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }
        if (perWalletModelData == null || perWalletModelData.getWallet() == null) {
            return walletData;
        }
        Set<Transaction> transactions = perWalletModelData.getWallet().getTransactions(false, false);

        if (transactions != null) {
            for (Transaction loopTransaction : transactions) {
                WalletTableData walletDataRow = new WalletTableData(loopTransaction);
                walletData.add(walletDataRow);
                walletDataRow.setCredit(loopTransaction.getValueSentToMe(perWalletModelData.getWallet()));
                try {
                    walletDataRow.setDebit(loopTransaction.getValueSentFromMe(perWalletModelData.getWallet()));
                } catch (ScriptException e) {
                    log.error(e.getMessage(), e);

                }
                List<TransactionInput> transactionInputs = loopTransaction.getInputs();
                List<TransactionOutput> transactionOutputs = loopTransaction.getOutputs();
                if (transactionInputs != null) {
                    TransactionInput firstInput = transactionInputs.get(0);
                    if (firstInput != null) {
                        walletDataRow.setDescription(createDescription(perWalletModelData.getWallet(), transactionInputs,
                                transactionOutputs, walletDataRow.getCredit(), walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(loopTransaction));
                walletDataRow.setHeight(workOutHeight(loopTransaction));
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
     * add the receiving addresses of all the keys of the specified wallet
     */
    public void createAddressBookReceivingAddresses(String walletFilename) {
        if (walletFilename == null) {
            return;
        }

        PerWalletModelData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }

        if (!(perWalletModelData == null)) {
            ArrayList<ECKey> keyChain = perWalletModelData.getWallet().keychain;
            if (keyChain != null) {
                MultiBitService multiBitService = controller.getMultiBitService();
                if (multiBitService != null) {
                    NetworkParameters networkParameters = multiBitService.getNetworkParameters();
                    if (networkParameters != null) {
                        if (perWalletModelData.getWalletInfo() != null) {
                            // clear the existing receiving addresses
                            perWalletModelData.getWalletInfo().getReceivingAddresses().clear();
                            for (ECKey key : keyChain) {
                                Address address = key.toAddress(controller.getMultiBitService().getNetworkParameters());
                                perWalletModelData.getWalletInfo().addReceivingAddressOfKey(address);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * create a description for a transaction
     * 
     * @param transactionInputs
     * @param transactionOutputs
     * @param credit
     * @param debit
     * @return
     */
    public String createDescription(Wallet wallet, List<TransactionInput> transactionInputs,
            List<TransactionOutput> transactionOutputs, BigInteger credit, BigInteger debit) {
        String toReturn = "";

        PerWalletModelData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (wallet.equals(loopPerWalletModelData.getWallet())) {
                    perWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }
        if (perWalletModelData == null) {
            return toReturn;
        }

        TransactionOutput myOutput = null;
        TransactionOutput theirOutput = null;
        if (transactionOutputs != null) {
            for (TransactionOutput transactionOutput : transactionOutputs) {
                if (transactionOutput != null && transactionOutput.isMine(perWalletModelData.getWallet())) {
                    myOutput = transactionOutput;
                }
                if (transactionOutput != null && !transactionOutput.isMine(perWalletModelData.getWallet())) {
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

                String label = null;
                if (perWalletModelData.getWalletInfo() != null) {
                    label = perWalletModelData.getWalletInfo().lookupLabelForReceivingAddress(addressString);
                }
                if (label != null && label != "") {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescriptionWithLabel",
                            new Object[] { addressString, label });
                } else {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescription",
                            new Object[] { addressString });
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);

            }
        }

        if (debit != null && debit.compareTo(BigInteger.ZERO) > 0) {
            // debit
            try {
                // see if the address is a known sending address
                if (theirOutput != null) {
                    String addressString = theirOutput.getScriptPubKey().getToAddress().toString();
                    String label = null;
                    if (perWalletModelData.getWalletInfo() != null) {
                        label = perWalletModelData.getWalletInfo().lookupLabelForSendingAddress(addressString);
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
                log.error(e.getMessage(), e);

            }
        }

        return toReturn;
    }

    /**
     * work out the transaction date
     * 
     * @param transaction
     * @return Date date of transaction
     */
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

    /**
     * work out the height of the block chain in which the transaction appears
     * 
     * @param transaction
     * @return
     */
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

    public void setActiveWalletInfo(WalletInfo walletInfo) {
        activeWalletModelData.setWalletInfo(walletInfo);
    }

    public List<PerWalletModelData> getPerWalletModelDataList() {
        return perWalletModelDataList;
    }

    public PerWalletModelData getPerWalletModelDataByWalletFilename(String walletFilename) {
        if (walletFilename == null) {
            return null;
        }
        
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    return loopPerWalletModelData;
                }
            }
        }
        return null;
    }
}
