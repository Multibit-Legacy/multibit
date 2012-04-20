/**
 * Copyright 2011 multibit.org
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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.multibit.controller.MultiBitController;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.View;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Sha256Hash;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.Wallet.BalanceType;
import com.google.bitcoin.core.WalletEventListener;
import com.google.bitcoin.store.BlockStoreException;

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

    // constants used in the multibit.properties

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
    
    // ticker
    public static final String TICKER_SHOW = "tickerShow";
    public static final String TICKER_COLUMNS_TO_SHOW = "tickerColumnsToShow";
    public static final String TICKER_FIRST_ROW_EXCHANGE = "tickerFirstRowExchange";
    public static final String TICKER_FIRST_ROW_CURRENCY = "tickerFirstRowCurrency";
    public static final String TICKER_SHOW_SECOND_ROW = "tickerShowSecondRow";
    public static final String TICKER_SECOND_ROW_EXCHANGE = "tickerSecondRowExchange";
    public static final String TICKER_SECOND_ROW_CURRENCY = "tickerSecondRowCurrency";
    
    // wallets, open wallet and save wallet as dialog
    public static final String ACTIVE_WALLET_FILENAME = "selectedWalletFilename";
    public static final String WALLET_FILENAME_PREFIX = "walletFilename.";
    public static final String WALLET_DESCRIPTION_PREFIX = "walletDescription.";
    public static final String NUMBER_OF_WALLETS = "numberOfWallets";
    public static final String GRAB_FOCUS_FOR_ACTIVE_WALLET = "grabFocusForActiveWallet";

    // send bitcoin and send bitcoin confirm
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";
    public static final String SEND_FEE = "sendFee";
    public static final String SEND_PERFORM_PASTE_NOW = "sendPerformPasteNow";
    public static final String SHOW_SIDE_PANEL = "showSidePanel";
    public static final String DISPLAY_AS_SWATCH = "displayAsSwatch";
    public static final String DISPLAY_AS_QR_CODE = "displayAsQRcode";

    // open uri
    public static final String OPEN_URI_SHOW_DIALOG = "openUriShowDialog";
    public static final String OPEN_URI_USE_URI = "openUriUseUri";
    public static final String OPEN_URI_ADDRESS = "openUriAddress";
    public static final String OPEN_URI_LABEL = "openUriLabel";
    public static final String OPEN_URI_AMOUNT = "openUriAmount";
    public static final String BRING_TO_FRONT = "bringToFront";

    // default is min fee of 0.0001 BTC
    public static final BigInteger SEND_FEE_DEFAULT = new BigInteger("50000");
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

    // validation
    public static final String VALIDATION_ADDRESS_IS_INVALID = "validationAddressIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_INVALID = "validationAmountIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_MISSING = "validationAmountIsMissing";
    public static final String VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO = "validationAmountIsNegativeOrZero";
    public static final String VALIDATION_NOT_ENOUGH_FUNDS = "validationNotEnoughFunds";
    public static final String VALIDATION_ADDRESS_VALUE = "validationAddressValue";
    public static final String VALIDATION_AMOUNT_VALUE = "validationAmountValue";

    public static final String WALLET_FILE_EXTENSION = "wallet";

    // private key import and export
    public static final String PRIVATE_KEY_FILE_EXTENSION = "key";
    public static final String PRIVATE_KEY_FILENAME = "privateKeyFilename";

    //Blockchain.info
    public static final String BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX = "aes.json";
    public static final String BLOCKCHAIN_WALLET_PLAIN_SUFFIX = "json";

    // connect to single node
    public static final String SINGLE_NODE_CONNECTION = "singleNodeConnection";

    // sizes and last modified dates of files
    public static final String WALLET_FILE_SIZE = "walletFileSize";
    public static final String WALLET_FILE_LAST_MODIFIED = "walletFileLastModified";

    public static final String WALLET_INFO_FILE_SIZE = "walletInfoFileSize";
    public static final String WALLET_INFO_FILE_LAST_MODIFIED = "walletInfoFileLastModified";

    // merchant menu
    public static final String SHOW_MERCHANT_MENU = "showMerchantMenu";

    // user preference font
    public static final String FONT = "font";
    public static final String FONT_NAME = "fontName";
    public static final String FONT_STYLE = "fontStyle";
    public static final String FONT_SIZE = "fontSize";

    public static final String PREVIOUS_FONT_NAME = "previousFontName";
    public static final String PREVIOUS_FONT_STYLE = "previousFontStyle";
    public static final String PREVIOUS_FONT_SIZE = "previousFontSize";

    // user preferences undo
    public static final String PREVIOUS_OPEN_URI_SHOW_DIALOG = "previousOpenUriShowDialog";
    public static final String PREVIOUS_OPEN_URI_USE_URI = "previousOpenUriUseUri";
    public static final String PREVIOUS_SEND_FEE = "previousSendFee";
    public static final String PREVIOUS_USER_LANGUAGE_CODE = "previousLanguageCode";
    public static final String PREVIOUS_UNDO_CHANGES_TEXT = "previousUndoChangesText";
    public static final String CAN_UNDO_PREFERENCES_CHANGES = "canUndoPreferencesChanges";

    // main controller class
    private final MultiBitController controller;

    // user preferences
    private Properties userPreferences;

    /**
     * List of each wallet's total model data
     */
    private List<PerWalletModelData> perWalletModelDataList;

    /**
     * The current active wallet
     */
    private PerWalletModelData activeWalletModelData;
    
    /**
     * The currently displayed view. One of the View constants.
     */
    private int currentView;
    
    /**
     * Holds exchange Data
     */
    private ExchangeData exchangeData = new ExchangeData(); // new up an empty ExchangeData object
  
    public MultiBitModel(MultiBitController controller) {
        this(controller, new Properties());
    }

    public MultiBitModel(MultiBitController controller, Properties userPreferences) {
        this.controller = controller;
        this.userPreferences = userPreferences;

        perWalletModelDataList = new LinkedList<PerWalletModelData>();

        activeWalletModelData = new PerWalletModelData();
        perWalletModelDataList.add(activeWalletModelData);
        

        // initialize everything to look at the stored opened view
        // if no properties passed in just initialize to the default view
        int initialView = View.DEFAULT_VIEW;
        if (userPreferences != null) {
            String viewString = (String) userPreferences.get(MultiBitModel.SELECTED_VIEW);
            if (viewString != null) {
                try {
                    int initialViewInProperties = Integer.parseInt(viewString);

                    // do not open obsolete views
                    if (View.OPEN_WALLET_VIEW != initialViewInProperties && View.SAVE_WALLET_AS_VIEW != initialViewInProperties
                            && View.SEND_BITCOIN_CONFIRM_VIEW != initialViewInProperties) {
                        initialView = initialViewInProperties;
                    }
                } catch (NumberFormatException nfe) {
                    // carry on
                }
            }
        }

        setCurrentView(initialView);
        log.debug("Initial view from properties file is '" + getCurrentView() + "'");
        
        controller.setModel(this);
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
     * Remove the specified perWalletModelData
     * Note that this does not remove any backing wallet or wallet info files
     * 
     * Removal is determined by matching the wallet filename
     * Use FileHandler to do that
     * 
     * @param perWalletModeData
     */
    public void remove(PerWalletModelData perWalletModelDataToRemove) {
        if (perWalletModelDataToRemove == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (perWalletModelDataToRemove.getWalletFilename().equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelDataList.remove(loopPerWalletModelData);
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
                public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
                    controller.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
                }

                @Override
                public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {
                    controller.onCoinsSent(wallet, transaction, prevBalance, newBalance);
                }

                @Override
                public void onReorganize(Wallet wallet) {
                    controller.onReorganise(wallet);
                }

                public void onTransactionConfidenceChanged(Wallet wallet, Transaction tx) {
                   controller.onTransactionConfidenceChanged(wallet, tx); 
                }
            });
        }

        createWalletData(walletFilename);
        createAddressBookReceivingAddresses(walletFilename);

        return newPerWalletModelData;
    }

    /**
     * Process a new coin
     * 
     * @param wallet
     * @param transaction
     */
    public void processNewCoin(Wallet wallet, Transaction transaction) {
        // loop through all the wallets, updating them as required with the new
        // transaction
        log.debug("processNewCoin is processing transaction " + transaction.toString());
        try {
            java.util.List<PerWalletModelData> perWalletModelDataList = getPerWalletModelDataList();

            if (perWalletModelDataList != null) {
                for (PerWalletModelData perWalletModelData : perWalletModelDataList) {
                    try {
                        Wallet loopWallet = perWalletModelData.getWallet();
                        if (loopWallet.isTransactionRelevant(transaction, true)) {
                            // the perWalletModelData is marked as dirty

                            // check to see if the transaction is already in the
                            // wallet
                            if (loopWallet.getTransaction(transaction.getHash()) == null) {
                                log.debug("processNewCoin is receivingPending");
                                loopWallet.receivePending(transaction);
                            }
                            perWalletModelData.setDirty(true);
                            log.debug("Marking wallet '" + perWalletModelData.getWalletFilename() + "' as dirty.");
                            controller.fireDataChanged();
                        }
                    } catch (VerificationException e) {
                        e.printStackTrace();
                    }
                }
            }
        } catch (ScriptException e) {
            // If we didn't understand the scriptSig, just log it
            log.error(e.getMessage(), e);
        }
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
    public ArrayList<WalletTableData> createWalletData(String walletFilename) {
        ArrayList<WalletTableData> walletData = new ArrayList<WalletTableData>();

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
     * @return A description of the transaction
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
        Collection<Sha256Hash> appearsIn = transaction.getAppearsInHashes();
        if (appearsIn != null) {
            if (!appearsIn.isEmpty()) {
                Iterator<Sha256Hash> iterator = appearsIn.iterator();
                // just take the first i.e. ignore impact of side chains
                if (iterator.hasNext()) {
                    Sha256Hash appearsInHash = iterator.next();
                    StoredBlock appearsInStoredBlock;
                    try {
                        if (controller != null && controller.getMultiBitService() != null
                                && controller.getMultiBitService().getBlockStore() != null) {
                            appearsInStoredBlock = controller.getMultiBitService().getBlockStore().get(appearsInHash);
                            Block appearsInBlock = appearsInStoredBlock.getHeader();
                            // set the time of the block to be the time of the
                            // transaction - TODO get transaction time
                            return new Date(appearsInBlock.getTimeSeconds() * 1000);
                        }
                    } catch (BlockStoreException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
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

    public int getCurrentView() {
        return currentView;
    }

    public void setCurrentView(int view) {
        this.currentView = view;
        setUserPreference(MultiBitModel.SELECTED_VIEW, "" + view);
    }

	public ExchangeData getExchangeData() {
		return exchangeData;
	}

	public void setExchangeData(ExchangeData exchangeData) {
		this.exchangeData = exchangeData;
	}
    
    
}
