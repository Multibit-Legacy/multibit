/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.model.bitcoin;

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
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.store.BlockStoreException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import org.multibit.Localiser;
import org.multibit.model.AbstractModel;
import org.multibit.model.ModelEnum;
import org.multibit.model.bitcoin.wallet.WalletInfoData;
import org.multibit.model.bitcoin.wallet.WalletTableData;
import org.multibit.model.bitcoin.wallet.WalletData;
import org.multibit.store.ReplayableBlockStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author Cameron Garnham
 */
public class BitcoinModel extends AbstractModel {

    private static final Logger log = LoggerFactory.getLogger(BitcoinModel.class);


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

    // The number of serialised and protobuf2 wallets in the multibit.properties.
    public static final String EARLY_WALLET_FILENAME_PREFIX = "walletFilename.";
    public static final String NUMBER_OF_EARLY_WALLETS = "numberOfWallets";
    public static final String PROTOBUF3_WALLET_FILENAME_PREFIX = "protobuf3WalletFilename.";
    public static final String NUMBER_OF_PROTOBUF3_WALLETS = "numberfProtobuf3Wallets";
    public static final String WALLET_ORDER_TOTAL = "walletOrderTotal";
    public static final String WALLET_ORDER_PREFIX = "walletOrder.";

    // Send bitcoin and send bitcoin confirm.
    public static final String SEND_ADDRESS = "sendAddress";
    public static final String SEND_LABEL = "sendLabel";
    public static final String SEND_AMOUNT = "sendAmount";
    public static final String SEND_FEE = "sendFee";
    public static final String SEND_PERFORM_PASTE_NOW = "sendPerformPasteNow";
    public static final String SHOW_SIDE_PANEL = "showSidePanel";
    public static final String DISPLAY_AS_SWATCH = "displayAsSwatch";
    public static final String DISPLAY_AS_QR_CODE = "displayAsQRcode";
    public static final int MINIMUM_NUMBER_OF_CONNECTED_PEERS_BEFORE_SEND_IS_ENABLED = 2;

    // Open bitcoin URI.
    public static final String OPEN_URI_SHOW_DIALOG = "openUriShowDialog";
    public static final String OPEN_URI_USE_URI = "openUriUseUri";
    public static final String OPEN_URI_ADDRESS = "openUriAddress";
    public static final String OPEN_URI_LABEL = "openUriLabel";
    public static final String OPEN_URI_AMOUNT = "openUriAmount";
    public static final String BRING_TO_FRONT = "bringToFront";

    public static final String PREVIOUS_OPEN_URI_SHOW_DIALOG = "previousOpenUriShowDialog";
    public static final String PREVIOUS_OPEN_URI_USE_URI = "previousOpenUriUseUri";
    public static final String PREVIOUS_SEND_FEE = "previousSendFee";
    
    // Default fee.
    public static final BigInteger SEND_FEE_DEFAULT = new BigInteger("100000");
    
    // Minimum fee.
    public static final BigInteger SEND_MINIMUM_FEE = new BigInteger("10000");
    public static final String SEND_WAS_SUCCESSFUL = "sendWasSuccessful";
    public static final String SEND_ERROR_MESSAGE = "sendErrorMessage";

    // Receive bitcoin.
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
    public static final String VALIDATION_NOT_ENOUGH_FUNDS = "validationNotEnoughFunds";
    public static final String VALIDATION_ADDRESS_VALUE = "validationAddressValue";
    public static final String VALIDATION_AMOUNT_VALUE = "validationAmountValue";
    public static final String WALLET_FILE_EXTENSION = "wallet";

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
    public static final int UNKNOWN_NUMBER_OF_CONNECTD_PEERS = -1;

    // Wallet migration.
    public static final String LAST_FAILED_MIGRATE_VERSION = "lastFailedMigrateVersion";
       
    // Wallet backup.
    public static final String WALLET_BACKUP_FILE = "walletBackupFile";
       
    

    
    /**
     * The number of peers connected.
     */
    private int numberOfConnectedPeers = UNKNOWN_NUMBER_OF_CONNECTD_PEERS;
    /**
     * List of each wallet's total model data.
     */
    private List<WalletData> perWalletModelDataList;
    /**
     * The current active wallet.
     */
    private WalletData activeWalletModelData;
    
    private NetworkParametersEnum networkPramEnum;
    

    
    public BitcoinModel(){
        this(null);
    }

    public BitcoinModel(NetworkParametersEnum networkPramEnum) {
        
        this.networkPramEnum = (null != networkPramEnum) ? networkPramEnum : NetworkParametersEnum.PRODUCTION_NETWORK;
    }

    @Override
    public ModelEnum getModelEnum() {
        return ModelEnum.BITCOIN;
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
     * @return
     */
    public void setActiveWalletPreference(String key, String value) {
        if (SEND_AMOUNT.equals(key)) {
            if (value.indexOf(",") > -1) {
                boolean bad = true;
                bad = !bad;
            }
        }
        if (activeWalletModelData.getWalletInfo() != null && value != null) {
            activeWalletModelData.getWalletInfo().putProperty(key, value);
            activeWalletModelData.setDirty(true);
        }
    }

    /**
     * Get the estimated balance of the active wallet.
     *
     * @return
     */
    public BigInteger getActiveWalletEstimatedBalance() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(Wallet.BalanceType.ESTIMATED);
        }
    }

    /**
     * Get the available balance (plus boomeranged change) of the active wallet.
     *
     * @return
     */
    public BigInteger getActiveWalletAvailableBalanceWithBoomerangChange() {
        if (activeWalletModelData.getWallet() == null) {
            return BigInteger.ZERO;
        } else {
            return activeWalletModelData.getWallet().getBalance(Wallet.BalanceType.AVAILABLE_WITH_BOOMERANG_CHANGE);
        }
    }

    /**
     * Get the wallet data for the active wallet.
     *
     * @return
     */
    public List<WalletTableData> getActiveWalletWalletData() {
        return activeWalletModelData.getWalletTableDataList();
    }

    /**
     * Get the wallet info for the active wallet.
     *
     * @return
     */
    public WalletInfoData getActiveWalletWalletInfo() {
        return activeWalletModelData.getWalletInfo();
    }

    /**
     * Get the active wallet.
     *
     * @return
     */
    public Wallet getActiveWallet() {
        return activeWalletModelData.getWallet();
    }

    /**
     * Set the active wallet, given a wallet filename.
     *
     * @param wallet
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
     * @param perWalletModeData
     */
    public void removeWallet(WalletData perWalletModelDataToRemove) {
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
    }

    /**
     * Set a wallet description, given a wallet filename.
     *
     * @param wallet
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
    public WalletData addWallet(Wallet wallet, String walletFilename, Localiser localiser, ReplayableBlockStore blockStore) {
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

        createWalletData(walletFilename, localiser, blockStore);
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
    public ArrayList<WalletTableData> createActiveWalletData(BitcoinModel model, Localiser localiser, ReplayableBlockStore blockStore) {
        return createWalletDataInternal(model.getActivePerWalletModelData(),localiser,blockStore);
    }
    
    /**
     * Convert the wallet info into walletdata records as they are easier
     * to show to the user in tabular form.
     */
    public ArrayList<WalletTableData> createWalletData(String walletFilename, Localiser localiser, ReplayableBlockStore blockStore) {
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
        
        return createWalletDataInternal(perWalletModelData, localiser, blockStore);
    }

    public ArrayList<WalletTableData> createWalletDataInternal(WalletData perWalletModelData, Localiser localiser, ReplayableBlockStore blockStore) {
        ArrayList<WalletTableData> walletData = new ArrayList<WalletTableData>();

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
                                transactionOutputs, walletDataRow.getCredit(), walletDataRow.getDebit(), localiser));
                    }
                }
                walletDataRow.setDate(createDate(loopTransaction, blockStore));
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
            ArrayList<ECKey> keyChain = perWalletModelData.getWallet().keychain;
            if (keyChain != null) {
                NetworkParameters networkParameters = getNetworkParameters();
                if (networkParameters != null) {
                    if (perWalletModelData.getWalletInfo() != null) {
                        // clear the existing receiving addresses
                        perWalletModelData.getWalletInfo().getReceivingAddresses().clear();
                        for (ECKey key : keyChain) {
                            Address address = key.toAddress(getNetworkParameters());
                            perWalletModelData.getWalletInfo().addReceivingAddressOfKey(address);
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
    public String createDescription(Wallet wallet, List<TransactionInput> transactionInputs,
            List<TransactionOutput> transactionOutputs, BigInteger credit, BigInteger debit, Localiser localiser) {
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
                if (label != null && label != "") {
                    toReturn = localiser.getString("multiBitModel.creditDescriptionWithLabel",
                            new Object[]{addressString, label});
                } else {
                    toReturn = localiser.getString("multiBitModel.creditDescription",
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
                    String addressString = theirOutput.getScriptPubKey().getToAddress().toString();
                    String label = null;
                    if (perWalletModelData.getWalletInfo() != null) {
                        label = perWalletModelData.getWalletInfo().lookupLabelForSendingAddress(addressString);
                    }
                    if (label != null && label != "") {
                        toReturn = localiser.getString("multiBitModel.debitDescriptionWithLabel",
                                new Object[]{addressString, label});
                    } else {
                        toReturn = localiser.getString("multiBitModel.debitDescription",
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
    private Date createDate(Transaction transaction, ReplayableBlockStore blockStore) {
        // If transaction has altered date - return that.
        if (transaction.getUpdateTime() != null) {
            return transaction.getUpdateTime();
        }

        // Other wise return the date of the block it first appeared in.
        Collection<Sha256Hash> appearsIn = transaction.getAppearsInHashes();
        if (appearsIn != null) {
            if (!appearsIn.isEmpty()) {
                Iterator<Sha256Hash> iterator = appearsIn.iterator();
                // just take the first i.e. ignore impact of side chains
                if (iterator.hasNext()) {
                    Sha256Hash appearsInHash = iterator.next();
                    StoredBlock appearsInStoredBlock;
                    try {
                        if (blockStore != null) {
                            appearsInStoredBlock = blockStore.get(appearsInHash);
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

        switch (this.networkPramEnum) {
            case PRODUCTION_NETWORK:
            return NetworkParameters.prodNet();
            case TEST_NETWORK:
                return NetworkParameters.testNet();
            case OLD_TEST_NETWORK:
                return NetworkParameters.oldTestNet();
        }

        return null;
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
}
