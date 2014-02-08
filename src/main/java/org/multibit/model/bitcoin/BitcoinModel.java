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
package org.multibit.model.bitcoin;

import com.google.dogecoin.core.*;
import com.google.dogecoin.core.Wallet.BalanceType;
import com.google.dogecoin.store.BlockStoreException;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.AbstractModel;
import org.multibit.model.ModelEnum;
import org.multibit.model.core.CoreModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.*;

/**
 * Model containing the MultiBit data.
 * 
 * Most of the methods act on the single, active wallet in the model.
 * 
 * @author jim
 * 
 */
public class BitcoinModel extends AbstractModel<CoreModel> {

      private static final Logger log = LoggerFactory.getLogger(BitcoinModel.class);

    // Constants used in the multibit.properties.

    // MultiBit start up.
    public static final String TEST_OR_PRODUCTION_NETWORK = "testOrProductionNetwork";
    public static final String TEST_NETWORK_VALUE = "test";
    public static final String TESTNET3_VALUE = "testnet3";
    public static final String PRODUCTION_NETWORK_VALUE = "production";
    public static final String WALLET_FILENAME = "walletFilename";

    // Wallets, open wallet and save wallet as dialog.
    public static final String GRAB_FOCUS_FOR_ACTIVE_WALLET = "grabFocusForActiveWallet";
    public static final String ACTIVE_WALLET_FILENAME = "selectedWalletFilename";
    public static final String WALLET_DESCRIPTION_PREFIX = "walletDescription.";
    public static final String SHOW_DELETE_WALLET = "showDeleteWallet";

    // The number of serialised and protobuf2 wallets in the multibit.properties.
    public static final String EARLY_WALLET_FILENAME_PREFIX = "walletFilename.";
    public static final String NUMBER_OF_EARLY_WALLETS = "numberOfWallets";

    public static final String PROTOBUF3_WALLET_FILENAME_PREFIX = "protobuf3WalletFilename.";
    public static final String NUMBER_OF_PROTOBUF3_WALLETS = "numberfProtobuf3Wallets";

    public static final String WALLET_ORDER_TOTAL = "walletOrderTotal";
    public static final String WALLET_ORDER_PREFIX = "walletOrder.";

    // Send dogecoin and send dogecoin confirm.
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";
    public static final String SEND_FEE = "sendFee";
    public static final String SEND_PERFORM_PASTE_NOW = "sendPerformPasteNow";
    public static final String SHOW_SIDE_PANEL = "showSidePanel";
    public static final String DISPLAY_AS_SWATCH = "displayAsSwatch";
    public static final String DISPLAY_AS_QR_CODE = "displayAsQRcode";
    
    public static final int MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED = 2;

    // Open dogecoin URI.
    public static final String OPEN_URI_SHOW_DIALOG = "openUriShowDialog";
    public static final String OPEN_URI_USE_URI = "openUriUseUri";
    public static final String OPEN_URI_ADDRESS = "openUriAddress";
    public static final String OPEN_URI_LABEL = "openUriLabel";
    public static final String OPEN_URI_AMOUNT = "openUriAmount";
    public static final String BRING_TO_FRONT = "bringToFront";

    // Default fee and feePerKB
    public static final BigInteger SEND_FEE_DEFAULT = new BigInteger("100000000");
    public static final BigInteger SEND_FEE_PER_KB_DEFAULT = new BigInteger("100000000");
    
    // Minimum fee.
    public static final BigInteger SEND_MINIMUM_FEE = new BigInteger("100000000");

    // Maximum fee.
    public static final BigInteger SEND_MAXIMUM_FEE = new BigInteger("100000000000"); // 1000 DOGE. Hope it won't ever go above du to bogus transactions.

    // Receive dogecoin.
    public static final String IS_RECEIVE_BITCOIN = "isReceiveBitcoin";
    public static final String RECEIVE_ADDRESS = "receiveAddress";
    public static final String RECEIVE_LABEL = "receiveLabel";
    public static final String RECEIVE_AMOUNT = "receiveAmount";
    public static final String RECEIVE_NEW_KEY = "receiveNewKey"; // to delete

    // Validation.
    public static final String VALIDATION_ADDRESS_IS_INVALID = "validationAddressIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_INVALID = "validationAmountIsInvalid";
    public static final String VALIDATION_AMOUNT_IS_MISSING = "validationAmountIsMissing";
    public static final String VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO = "validationAmountIsNegativeOrZero";
    public static final String VALIDATION_AMOUNT_IS_TOO_SMALL = "validationAmountIsTooSmall";
    public static final String VALIDATION_NOT_ENOUGH_FUNDS = "validationNotEnoughFunds";
    public static final String VALIDATION_ADDRESS_VALUE = "validationAddressValue";
    public static final String VALIDATION_AMOUNT_VALUE = "validationAmountValue";

    public static final String WALLET_FILE_EXTENSION = "wallet";
    public static final String CSV_FILE_EXTENSION = "csv";

    // Private key import and export.
    public static final String PRIVATE_KEY_FILE_EXTENSION = "key";
    public static final String PRIVATE_KEY_FILENAME = "privateKeyFilename";

    // Blockchain.info support.
    public static final String BLOCKCHAIN_WALLET_ENCRYPTED_SUFFIX = "aes.json";
    public static final String BLOCKCHAIN_WALLET_PLAIN_SUFFIX = "json";

    // Connect to nodes.
    @Deprecated
    public static final String SINGLE_NODE_CONNECTION = "singleNodeConnection";
    public static final String PEERS = "peers";

    
    // Sizes and last modified dates of files.
    public static final String WALLET_FILE_SIZE = "walletFileSize";
    public static final String WALLET_FILE_LAST_MODIFIED = "walletFileLastModified";

    public static final String WALLET_INFO_FILE_SIZE = "walletInfoFileSize";
    public static final String WALLET_INFO_FILE_LAST_MODIFIED = "walletInfoFileLastModified";

    // User preferences undo.
    public static final String PREVIOUS_OPEN_URI_SHOW_DIALOG = "previousOpenUriShowDialog";
    public static final String PREVIOUS_OPEN_URI_USE_URI = "previousOpenUriUseUri";
    public static final String PREVIOUS_SEND_FEE = "previousSendFee";
       
    // Wallet backup.
    public static final String WALLET_BACKUP_FILE = "walletBackupFile";

    // AlertManager and versions
    public static final String ALERT_MANAGER_NEW_VERSION_VALUE = "alertManagerNewVersionValue";
    public static final String ALERT_MANAGER_NEW_VERSION_SEEN_COUNT = "alertManagerNewVersionSeenCount";
       
    
    /**
     * List of each wallet's total model data.
     */
    private List<WalletData> perWalletModelDataList;

    /**
     * The current active wallet.
     */
    private WalletData activeWalletModelData;
    


    public static final int UNKNOWN_NUMBER_OF_CONNECTD_PEERS = -1;
    
    /**
     * The number of peers connected.
     */
    private int numberOfConnectedPeers = UNKNOWN_NUMBER_OF_CONNECTD_PEERS;
    
    /**
     * Used to enable/ disable blinking of the SingleWalletPanels when language changes etc.
     */
    private boolean blinkEnabled = true;

    @SuppressWarnings("deprecation")
    public BitcoinModel(CoreModel coreModel) {
        super(coreModel);

        perWalletModelDataList = new LinkedList<WalletData>();

        activeWalletModelData = new WalletData();
        perWalletModelDataList.add(activeWalletModelData);
    }


    public WalletData getActivePerWalletModelData() {
        return activeWalletModelData;
    }

    /**
     * Get a wallet preference from the active wallet.
     *
     * @param key String key of property
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
     * Set a wallet preference from the active wallet.
     *
     */
    public void setActiveWalletPreference(String key, String value) {
        if (BitcoinModel.SEND_AMOUNT.equals(key)) {
            if (value.contains(",")) {
                boolean bad = true;
                bad = !bad;
            }
        }
        if (activeWalletModelData.getWalletInfo() != null && value != null) {
            activeWalletModelData.getWalletInfo().put(key, value);
            activeWalletModelData.setDirty(true);
        }
    }

    /**
     * Get the estimated balance of the active wallet.
     *
     * @return The estimated balance
     */
    public BigInteger getActiveWalletEstimatedBalance() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(BalanceType.ESTIMATED);
        }
    }

    /**
     * Get the available balance (plus boomeranged change) of the active wallet.
     *
     * @return the available balance
     */
    public BigInteger getActiveWalletAvailableBalance() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(BalanceType.AVAILABLE);
        }
    }

    /**
     * Get the wallet data for the active wallet.
     *
     * @return the table data list
     */
    public List<WalletTableData> getActiveWalletWalletData() {
        return activeWalletModelData.getWalletTableDataList();
    }

    /**
     * @return the wallet info for the active wallet
     */
    public WalletInfoData getActiveWalletWalletInfo() {
        return activeWalletModelData.getWalletInfo();
    }

    /**
     * @return the active wallet
     */
    public Wallet getActiveWallet() {
        return activeWalletModelData.getWallet();
    }

    /**
     * Set the active wallet, given a wallet filename.
     *
     * @param walletFilename the wallet filename
     */
    public void setActiveWalletByFilename(String walletFilename) {
        if (walletFilename == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    activeWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }
    }

    /**
     * Remove the specified perWalletModelData. Note that this does not remove
     * any backing wallet or wallet info files.
     *
     * Removal is determined by matching the wallet filename. Use FileHandler to
     * do that.
     *
     * @param perWalletModelDataToRemove The wallet data
     */
    public void remove(WalletData perWalletModelDataToRemove) {
        if (perWalletModelDataToRemove == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (perWalletModelDataToRemove.getWalletFilename().equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelDataList.remove(loopPerWalletModelData);
                    break;
                }
            }
        }
        
        // If there are no wallets, clear the activeWalletModelData.
        activeWalletModelData = new WalletData();
       
    }

    /**
     * Set a wallet description, given a wallet filename.
     *
     * @param walletFilename The wallet file name
     * @param walletDescription The wallet description
     */
    public void setWalletDescriptionByFilename(String walletFilename, String walletDescription) {
        if (walletFilename == null) {
            return;
        }
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    loopPerWalletModelData.setWalletDescription(walletDescription);
                    loopPerWalletModelData.setDirty(true);
                    break;
                }
            }
        }
    }

    /**
     * Add a new wallet to the list of managed wallets.
     */
    public WalletData addWallet(final BitcoinController bitcoinController, Wallet wallet, String walletFilename) {
        if (walletFilename == null) {
            return null;
        }

        // Check to see if it is already in the managed list - no need to add it
        // again if so.
        for (WalletData loopModelData : perWalletModelDataList) {
            if (walletFilename.equals(loopModelData.getWalletFilename())) {
                return loopModelData;
            }
        }

        WalletData newPerWalletModelData = new WalletData();
        newPerWalletModelData.setWallet(wallet);
        newPerWalletModelData.setWalletFilename(walletFilename);

        // Table row data used in displaying transactions - initially empty
        newPerWalletModelData.setWalletTableDataList(new ArrayList<WalletTableData>());

        // If it is the initial empty activeWalletModelData remove it.
        if (thereIsNoActiveWallet()) {
            perWalletModelDataList.remove(activeWalletModelData);
            activeWalletModelData = newPerWalletModelData;
        }

        perWalletModelDataList.add(newPerWalletModelData);

        // Wire up the controller as a wallet event listener.
        if (wallet != null) {
            wallet.addEventListener(bitcoinController);
        }

        createWalletTableData(bitcoinController, walletFilename);
        createAddressBookReceivingAddresses(walletFilename);

        return newPerWalletModelData;
    }

    /**
     * Get the active wallet filename.
     *
     *
     * @return
     */
    public String getActiveWalletFilename() {
        return activeWalletModelData.getWalletFilename();
    }

    /**
     * Convert the active wallet info into walletdata records as they are easier
     * to show to the user in tabular form.
     */
    public ArrayList<WalletTableData> createActiveWalletData(final BitcoinController bitcoinController) {
        return createWalletTableData(bitcoinController, this.getActivePerWalletModelData());
    }
    
    /**
     * Convert the wallet info into walletdata records as they are easier
     * to show to the user in tabular form.
     */
    public ArrayList<WalletTableData> createWalletTableData(final BitcoinController bitcoinController, String walletFilename) {
        ArrayList<WalletTableData> walletData = new ArrayList<WalletTableData>();

        if (walletFilename == null) {
            return walletData;
        }

        WalletData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }
        
        return createWalletTableData(bitcoinController, perWalletModelData);
    }

    public ArrayList<WalletTableData> createWalletTableData(final BitcoinController bitcoinController, WalletData perWalletModelData) {
        ArrayList<WalletTableData> walletData = new ArrayList<WalletTableData>();

        if (perWalletModelData == null || perWalletModelData.getWallet() == null) {
            return walletData;
        }
        
        Set<Transaction> transactions = perWalletModelData.getWallet().getTransactions(false);

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
                        walletDataRow.setDescription(createDescription(bitcoinController, perWalletModelData.getWallet(), transactionInputs,
                                transactionOutputs, walletDataRow.getCredit(), walletDataRow.getDebit()));
                    }
                }
                walletDataRow.setDate(createDate(bitcoinController, loopTransaction));
                walletDataRow.setHeight(workOutHeight(loopTransaction));
            }
        }

        // Run through all the walletdata to see if both credit and debit are
        // set (this means change was received).
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
     * Add the receiving addresses of all the keys of the specified wallet.
     */
    public void createAddressBookReceivingAddresses(String walletFilename) {
        if (walletFilename == null) {
            return;
        }

        WalletData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    perWalletModelData = loopPerWalletModelData;
                    break;
                }
            }
        }

        if (!(perWalletModelData == null)) {
            List<ECKey> keyChain = perWalletModelData.getWallet().getKeychain();
            if (keyChain != null) {
                NetworkParameters networkParameters = getNetworkParameters();
                if (networkParameters != null) {
                    if (perWalletModelData.getWalletInfo() != null) {
                        // Keep a copy of the existing receiving addresses - labels will be recycled.
                        List<WalletAddressBookData> currentReceivingAddresses = perWalletModelData.getWalletInfo().getReceivingAddresses();

                        // Clear the existing receiving addresses.
                        ArrayList<WalletAddressBookData> newReceivingAddresses = new ArrayList<WalletAddressBookData>();
                        perWalletModelData.getWalletInfo().setReceivingAddresses(newReceivingAddresses);

                        // Add the new receiving addresses from the keys, checking if there is an old label.
                        for (ECKey key : keyChain) {
                            Address address = key.toAddress(getNetworkParameters());
                            String addressString = address.toString();
                            WalletAddressBookData addressBookData = new WalletAddressBookData(null, addressString);

                            for (WalletAddressBookData loopAddressBookData : currentReceivingAddresses) {
                                if (loopAddressBookData.getAddress().equals(addressString)) {
                                    // Recycle label.
                                    addressBookData.setLabel(loopAddressBookData.getLabel());
                                    break;
                                }
                            }
                            perWalletModelData.getWalletInfo().addReceivingAddress(addressBookData, false);
                        }
                    }
                }
            }
        }
    }

    /**
     * Create a description for a transaction.
     *
     * @param transactionInputs
     * @param transactionOutputs
     * @param credit
     * @param debit
     * @return A description of the transaction
     */
    public String createDescription(final Controller controller, Wallet wallet, List<TransactionInput> transactionInputs,
            List<TransactionOutput> transactionOutputs, BigInteger credit, BigInteger debit) {
        String toReturn = "";

        WalletData perWalletModelData = null;
        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
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
            // Credit.
            try {
                String addressString = "";

                if (myOutput != null) {
                    Address toAddress = new Address(getNetworkParameters(), myOutput
                            .getScriptPubKey().getPubKeyHash());
                    addressString = toAddress.toString();
                }

                String label = null;
                if (perWalletModelData.getWalletInfo() != null) {
                    label = perWalletModelData.getWalletInfo().lookupLabelForReceivingAddress(addressString);
                }
                if (label != null && !label.equals("")) {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescriptionWithLabel",
                            new Object[]{addressString, label});
                } else {
                    toReturn = controller.getLocaliser().getString("multiBitModel.creditDescription",
                            new Object[]{addressString});
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);

            }
        }

        if (debit != null && debit.compareTo(BigInteger.ZERO) > 0) {
            // Debit.
            try {
                // See if the address is a known sending address.
                if (theirOutput != null) {
                    String addressString = theirOutput.getScriptPubKey().getToAddress(getNetworkParameters()).toString();
                    String label = null;
                    if (perWalletModelData.getWalletInfo() != null) {
                        label = perWalletModelData.getWalletInfo().lookupLabelForSendingAddress(addressString);
                    }
                    if (label != null && !label.equals("")) {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescriptionWithLabel",
                                new Object[]{addressString, label});
                    } else {
                        toReturn = controller.getLocaliser().getString("multiBitModel.debitDescription",
                                new Object[]{addressString});
                    }
                }
            } catch (ScriptException e) {
                log.error(e.getMessage(), e);
            }
        }
        return toReturn;
    }

    /**
     * Work out the transaction date.
     *
     * @param transaction
     * @return Date date of transaction
     */
    private Date createDate(final BitcoinController bitcoinController, Transaction transaction) {
        // If transaction has altered date - return that.
        if (transaction.getUpdateTime() != null) {
            return transaction.getUpdateTime();
        }

        // Other wise return the date of the block it first appeared in.
        Map<Sha256Hash, Integer> appearsIn = transaction.getAppearsInHashes();
        if (appearsIn != null) {
            if (!appearsIn.isEmpty()) {
                Iterator<Sha256Hash> iterator = appearsIn.keySet().iterator();
                // just take the first i.e. ignore impact of side chains
                if (iterator.hasNext()) {
                    Sha256Hash appearsInHash = iterator.next();
                    StoredBlock appearsInStoredBlock;
                    try {
                        if (bitcoinController != null && bitcoinController.getMultiBitService() != null
                                && bitcoinController.getMultiBitService().getBlockStore() != null) {
                            appearsInStoredBlock = bitcoinController.getMultiBitService().getBlockStore().get(appearsInHash);
                            Block appearsInBlock = appearsInStoredBlock.getHeader();
                            // Set the time of the block to be the time of the
                            // transaction - TODO get transaction time.
                            return new Date(appearsInBlock.getTimeSeconds() * 1000);
                        }
                    } catch (BlockStoreException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        return null;
    }

    /**
     * Work out the height of the block chain in which the transaction appears.
     *
     * @param transaction
     * @return
     */
    private int workOutHeight(Transaction transaction) {
        return -1; // -1 = we do not know. TODO probably needs replacing by height on TransactionConfidence.
    }

    public void setActiveWalletInfo(WalletInfoData walletInfo) {
        activeWalletModelData.setWalletInfo(walletInfo);
    }

    public List<WalletData> getPerWalletModelDataList() {
        return perWalletModelDataList;
    }

    public WalletData getPerWalletModelDataByWalletFilename(String walletFilename) {
        if (walletFilename == null) {
            return null;
        }

        if (perWalletModelDataList != null) {
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                if (walletFilename.equals(loopPerWalletModelData.getWalletFilename())) {
                    return loopPerWalletModelData;
                }
            }
        }
        return null;
    }
    
    public NetworkParameters getNetworkParameters() {
        // If test or production is not specified, default to production.
//        String testOrProduction = super.getUserPreference(BitcoinModel.TEST_OR_PRODUCTION_NETWORK);
//        if (testOrProduction == null) {
//            testOrProduction = BitcoinModel.PRODUCTION_NETWORK_VALUE;
//            super.setUserPreference(BitcoinModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
//        }
//        if (BitcoinModel.TEST_NETWORK_VALUE.equalsIgnoreCase(testOrProduction)) {
//            return NetworkParameters.testNet2();
//        } else if (BitcoinModel.TESTNET3_VALUE.equalsIgnoreCase(testOrProduction)) {
//            return NetworkParameters.testNet();
//        } else {
            return NetworkParameters.prodNet();
//        }
    }

    public boolean thereIsNoActiveWallet() {
        return activeWalletModelData == null
                || "".equals(activeWalletModelData.getWalletFilename()) || activeWalletModelData.getWalletFilename() == null;
    }

    public int getNumberOfConnectedPeers() {
        return numberOfConnectedPeers;
    }

    public void setNumberOfConnectedPeers(int numberOfConnectedPeers) {
        this.numberOfConnectedPeers = numberOfConnectedPeers;
    }

    public boolean isBlinkEnabled() {
        return blinkEnabled;
    }

    public void setBlinkEnabled(boolean blinkEnabled) {
        this.blinkEnabled = blinkEnabled;
    }
    
    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.BITCOIN;
    }
}
