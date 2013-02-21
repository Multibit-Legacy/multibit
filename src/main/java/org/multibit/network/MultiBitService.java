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
package org.multibit.network;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.SecureRandom;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.SimpleTimeZone;
import java.util.Stack;

import javax.swing.SwingWorker;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.MultiBit;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandlerException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.core.StatusEnum;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.ReplayableBlockStore;
import org.multibit.store.WalletVersionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.CheckpointManager;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.MultiBitBlockChain;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerAddress;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.VerificationException;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.Wallet.SendRequest;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.discovery.DnsDiscovery;
import com.google.bitcoin.discovery.IrcDiscovery;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;
import com.google.bitcoin.store.SPVBlockStore;


/**
 * <p>
 * MultiBitService encapsulates the interaction with the bitcoin netork
 * including: o Peers o Block chain download o sending / receiving bitcoins
 * 
 * The testnet can be slow or flaky as it's a shared resource. You can use the
 * <a href="http://sourceforge
 * .net/projects/bitcoin/files/Bitcoin/testnet-in-a-box/">testnet in a box</a>
 * to do everything purely locally.
 * </p>
 */
public class MultiBitService {
    private static final String TESTNET3_GENESIS_HASH = "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943";

    private static final Logger log = LoggerFactory.getLogger(MultiBitService.class);

    public static final int MAXIMUM_EXPECTED_LENGTH_OF_ALTERNATE_CHAIN = 6;

    public static final String MULTIBIT_PREFIX = "multibit";
    public static final String TESTNET_PREFIX = "testnet";
    public static final String TESTNET3_PREFIX = "testnet3";
    public static final String SEPARATOR = "-";

    public static final String BLOCKCHAIN_SUFFIX = ".blockchain";
    public static final String SPV_BLOCKCHAIN_SUFFIX = ".spvchain";
    public static final String CHECKPOINTS_SUFFIX = ".checkpoints";
    public static final String WALLET_SUFFIX = ".wallet";

    public static final String IRC_CHANNEL_TEST = "#bitcoinTEST";
    public static final String IRC_CHANNEL_TESTNET3 = "#bitcoinTEST3";

    static boolean restartListenerHasBeenAddedToPeerGroup = false;

    public Logger logger = LoggerFactory.getLogger(MultiBitService.class.getName());

    private Wallet wallet;

    private MultiBitPeerGroup peerGroup;

    private String blockchainFilename;

    private MultiBitBlockChain blockChain;

    private BlockStore blockStore;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private final NetworkParameters networkParameters;

    private SecureRandom secureRandom = new SecureRandom();

    private MultiBitCheckpointManager checkpointManager;
    private String checkpointsFilename;

    public static Date genesisBlockCreationDate;
    

    static {
        try {
            java.text.SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            java.util.Calendar cal = Calendar.getInstance(new SimpleTimeZone(0, "GMT"));
            format.setCalendar(cal);
            genesisBlockCreationDate = format.parse("2009-01-03 18:15:05");
        } catch (ParseException e) {
            // Will never happen.
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param controller
     *            MutliBitController
     */
    public MultiBitService(BitcoinController bitcoinController) {
        this(bitcoinController.getModel().getUserPreference(BitcoinModel.WALLET_FILENAME), bitcoinController);
    }

    /**
     * 
     * @param walletFilename
     *            filename of current wallet
     * @param controller
     *            MutliBitController
     */
    public MultiBitService(String walletFilename, BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;

        if (controller == null) {
            throw new IllegalStateException("controller cannot be null");
        }

        if (controller.getModel() == null) {
            throw new IllegalStateException("controller.getModel() cannot be null");
        }

        if (controller.getApplicationDataDirectoryLocator() == null) {
            throw new IllegalStateException("controller.getApplicationDataDirectoryLocator() cannot be null");
        }

        if (this.bitcoinController.getFileHandler() == null) {
            throw new IllegalStateException("controller.getFileHandler() cannot be null");
        }

        networkParameters = this.bitcoinController.getModel().getNetworkParameters();
        log.debug("Network parameters = " + networkParameters);

        try {
            // Load or create the blockStore..
            log.debug("Loading/ creating blockstore ...");
            blockStore = createBlockStore(null, false, false);
            log.debug("Blockstore is '" + blockStore + "'");

            log.debug("Creating blockchain ...");
            blockChain = new MultiBitBlockChain(networkParameters, blockStore);
            log.debug("Created blockchain '" + blockChain + "'");

            log.debug("Creating peergroup ...");
            createNewPeerGroup();
            log.debug("Created peergroup '" + peerGroup + "'");

            log.debug("Starting peergroup ...");
            peerGroup.start();
            log.debug("Started peergroup.");
        } catch (BlockStoreException e) {
            handleError(e);
        } catch (FileHandlerException e) {
            handleError(e);
        } catch (Exception e) {
            handleError(e);
        }
        
        FileInputStream stream = null;
        try {
            stream = new FileInputStream(checkpointsFilename);
            checkpointManager = new MultiBitCheckpointManager(networkParameters, stream);
        } catch (IOException e) {
            log.error("Error creating checkpointManager " + e.getClass().getName() + " " + e.getMessage());
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    log.error("Error tidying up checkpointManager creation" + e.getClass().getName() + " " + e.getMessage());
                }
            }
        }
    }

    private void handleError(Exception e) {
        controller.setOnlineStatus(StatusEnum.ERROR);
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString(
                "multiBitService.couldNotLoadBlockchain",
                new Object[] { blockchainFilename, e.getClass().getName() + " " + e.getMessage() })));
        log.error("Error creating MultiBitService " + e.getClass().getName() + " " + e.getMessage());
    }

    private BlockStore createBlockStore(Date checkpointDate, boolean createNew, boolean isReplay) throws BlockStoreException, IOException {
        BlockStore blockStore = null;

        String filePrefix = getFilePrefix();
        log.debug("filePrefix = " + filePrefix);

        String bobsBlockStoreFilename;
        String spvBlockStoreFilename;

        if ("".equals(controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
            bobsBlockStoreFilename = filePrefix + BLOCKCHAIN_SUFFIX;
            spvBlockStoreFilename = filePrefix + SPV_BLOCKCHAIN_SUFFIX;
            checkpointsFilename = filePrefix + CHECKPOINTS_SUFFIX;
        } else {
            bobsBlockStoreFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
                    + filePrefix + BLOCKCHAIN_SUFFIX;
            spvBlockStoreFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
                    + filePrefix + SPV_BLOCKCHAIN_SUFFIX;
            checkpointsFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
            + filePrefix + CHECKPOINTS_SUFFIX;
        }

        File bobsBlockStore = new File(bobsBlockStoreFilename);
        File spvBlockStore = new File(spvBlockStoreFilename);

        // Ensure there is a checkpoints file.
        File checkpointsFile = new File(checkpointsFilename);
        if (!checkpointsFile.exists()) {
            this.bitcoinController.getFileHandler().copyCheckpointsFromInstallationDirectory(checkpointsFilename);                
        }

        
        if (!spvBlockStore.exists() && (isReplay || !bobsBlockStore.exists())) {
            // If there is no SPVBlockStore and no ReplayableBlockStore create an SPVBlockStore and use it.
            blockchainFilename = spvBlockStoreFilename;
            
            blockStore = new SPVBlockStore(networkParameters, new File(blockchainFilename));

            // Load the existing checkpoint file and checkpoint from today.
            if (checkpointsFile.exists()) {
                FileInputStream stream = new FileInputStream(checkpointsFile);
                CheckpointManager.checkpoint(networkParameters, stream, blockStore, (new Date()).getTime() / 1000);
            }
        } else if (spvBlockStore.exists()) {
            // If there is an SPVStore prefer that.
            blockchainFilename = spvBlockStoreFilename;

            File blockchainFile = new File(blockchainFilename);
            if (createNew) {
                // Garbage collect any closed references to the blockchainFile.
                System.gc();
                blockchainFile.setWritable(true);
                boolean deletedOk = blockchainFile.delete();
                log.debug("Recreating SPV block store '{}' from disk", blockchainFilename + ", deletedOk = " + deletedOk);
            } else {
                log.debug("Opening SPV block store '{}' from disk", blockchainFilename);
            }
            blockStore = new SPVBlockStore(networkParameters, blockchainFile);

            // Load the existing checkpoint file, setting the checkpoint date if it is a replay.
            if (checkpointsFile.exists() && checkpointDate != null) {
                FileInputStream stream = new FileInputStream(checkpointsFile);
                CheckpointManager.checkpoint(networkParameters, stream, blockStore, checkpointDate.getTime() / 1000);
            }
        } else {
            // Use the existing bobsBlockStore.
            // (This will be used until the first replay/ import at which point the SPVStore will be used).
            blockchainFilename = bobsBlockStoreFilename;
            log.debug("Reading Replayable block store '{}' from disk", blockchainFilename);
            blockStore = new ReplayableBlockStore(networkParameters, new File(blockchainFilename), false);
        }
        return blockStore;
    }

    public void createNewPeerGroup() {
        peerGroup = new MultiBitPeerGroup(this.bitcoinController, networkParameters, blockChain);
        peerGroup.setFastCatchupTimeSecs(0); // genesis block
        peerGroup.setUserAgent("MultiBit", controller.getLocaliser().getVersionNumber());

        boolean peersSpecified = false;
        String singleNodeConnection = controller.getModel().getUserPreference(BitcoinModel.SINGLE_NODE_CONNECTION);
        String peers = controller.getModel().getUserPreference(BitcoinModel.PEERS);
        if (singleNodeConnection != null && !singleNodeConnection.equals("")) {
            try {
                peerGroup.addAddress(new PeerAddress(InetAddress.getByName(singleNodeConnection.trim())));
                peerGroup.setMaxConnections(1);
                peersSpecified = true;
            } catch (UnknownHostException e) {
                log.error(e.getMessage(), e);
            }
        } else if (peers != null && !peers.equals("")) {
            // Split using commas.
            String[] peerList = peers.split(",");
            if (peerList != null) {
                int numberOfPeersAdded = 0;

                for (int i = 0; i < peerList.length; i++) {
                    try {
                        peerGroup.addAddress(new PeerAddress(InetAddress.getByName(peerList[i].trim())));
                        numberOfPeersAdded++;
                    } catch (UnknownHostException e) {
                        log.error(e.getMessage(), e);
                    }
                }
                peerGroup.setMaxConnections(numberOfPeersAdded);
                peersSpecified = true;
            }
        }

        if (!peersSpecified) {
            // Use DNS for production, IRC for test.
            if (TESTNET3_GENESIS_HASH.equals(this.bitcoinController.getModel().getNetworkParameters().genesisBlock.getHashAsString())) {
                peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TESTNET3));
            } else if (NetworkParameters.testNet().equals(this.bitcoinController.getModel().getNetworkParameters())) {
                peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TEST));
            } else {
                peerGroup.addPeerDiscovery(new DnsDiscovery(networkParameters));
            }
        }
        // Add the controller as a PeerEventListener.
        peerGroup.addEventListener(this.bitcoinController.getPeerEventListener());

        // Add all existing wallets to the PeerGroup.
        if (controller != null && controller.getModel() != null) {
            List<WalletData> perWalletDataModels = this.bitcoinController.getModel().getPerWalletModelDataList();
            if (perWalletDataModels != null) {
                Iterator<WalletData> iterator = perWalletDataModels.iterator();
                if (iterator != null) {
                    while(iterator.hasNext()) {
                        WalletData perWalletModelData = iterator.next();
                        if (perWalletModelData != null && perWalletModelData.getWallet() != null) {
                            peerGroup.addWallet(perWalletModelData.getWallet());
                        }
                    }
                }
            }
        }
    }

    public static String getFilePrefix() {
        BitcoinController bitcoinController = MultiBit.getBitcoinController();
        // testnet3
        if (TESTNET3_GENESIS_HASH.equals(bitcoinController.getModel().getNetworkParameters().genesisBlock.getHashAsString())) {
            return MULTIBIT_PREFIX + SEPARATOR + TESTNET3_PREFIX;
        } else if (NetworkParameters.testNet().equals(bitcoinController.getModel().getNetworkParameters())) {
            return MULTIBIT_PREFIX + SEPARATOR + TESTNET_PREFIX;
        } else {
            return MULTIBIT_PREFIX;
        }
    }

    /**
     * Initialize wallet from the wallet filename.
     * 
     * @param walletFilename
     * @return perWalletModelData
     */
    public WalletData addWalletFromFilename(String walletFilename) throws IOException {
        WalletData perWalletModelDataToReturn = null;

        File walletFile = null;
        boolean walletFileIsADirectory = false;
        boolean newWalletCreated = false;

        if (walletFilename != null) {
            walletFile = new File(walletFilename);
            if (walletFile.isDirectory()) {
                walletFileIsADirectory = true;
            } else {

                perWalletModelDataToReturn = this.bitcoinController.getFileHandler().loadFromFile(walletFile);
                if (perWalletModelDataToReturn != null) {
                    wallet = perWalletModelDataToReturn.getWallet();
                }

            }
        }

        if (wallet == null || walletFilename == null || walletFilename.equals("") || walletFileIsADirectory) {
            // Use default wallet name - create if does not exist.
            if ("".equals(controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
                walletFilename = getFilePrefix() + WALLET_SUFFIX;
            } else {
                walletFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
                        + getFilePrefix() + WALLET_SUFFIX;
            }

            walletFile = new File(walletFilename);

            if (walletFile.exists()) {
                // Wallet file exists with default name.
                perWalletModelDataToReturn = this.bitcoinController.getFileHandler().loadFromFile(walletFile);
                if (perWalletModelDataToReturn != null) {
                    wallet = perWalletModelDataToReturn.getWallet();
                }

                newWalletCreated = true;
            } else {
                // Create a brand new wallet - by default unencrypted.
                wallet = new Wallet(networkParameters);
                ECKey newKey = new ECKey();
                wallet.keychain.add(newKey);

                perWalletModelDataToReturn = this.bitcoinController.getModel().addWallet(this.bitcoinController, wallet, walletFile.getAbsolutePath());

                // Create a wallet info.
                WalletInfoData walletInfo = new WalletInfoData(walletFile.getAbsolutePath(), MultiBitWalletVersion.PROTOBUF);
                perWalletModelDataToReturn.setWalletInfo(walletInfo);

                // Set a default description.
                String defaultDescription = controller.getLocaliser().getString("createNewWalletSubmitAction.defaultDescription");
                perWalletModelDataToReturn.setWalletDescription(defaultDescription);

                try {
                    this.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelDataToReturn, true);

                    newWalletCreated = true;
                } catch (WalletSaveException wse) {
                    log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                    MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
                } catch (WalletVersionException wve) {
                    log.error(wve.getClass().getCanonicalName() + " " + wve.getMessage());
                    MessageManager.INSTANCE.addMessage(new Message(wve.getClass().getCanonicalName() + " " + wve.getMessage()));
                }
            }
        }

        if (wallet != null) {
            // Add the keys for this wallet to the address book as receiving
            // addresses.
            ArrayList<ECKey> keys = wallet.keychain;
            if (keys != null) {
                if (!newWalletCreated) {
                    perWalletModelDataToReturn = this.bitcoinController.getModel().getPerWalletModelDataByWalletFilename(walletFilename);
                }
                if (perWalletModelDataToReturn != null) {
                    WalletInfoData walletInfo = perWalletModelDataToReturn.getWalletInfo();
                    if (walletInfo != null) {
                        for (ECKey key : keys) {
                            if (key != null) {
                                Address address = key.toAddress(networkParameters);
                                walletInfo.addReceivingAddressOfKey(address);
                            }
                        }
                    }
                }
            }

            // Add wallet to blockchain.
            if (blockChain != null) {
                blockChain.addWallet(wallet);
            } else {
                log.error("Could not add wallet '" + walletFilename + "' to the blockChain as the blockChain is missing.\n"
                        + "This is bad. MultiBit is currently looking for a blockChain at '" + blockchainFilename + "'");
            }

            // Add wallet to peergroup.
            if (peerGroup != null) {
                peerGroup.addWallet(wallet);
            } else {
                log.error("Could not add wallet '" + walletFilename + "' to the peerGroup as the peerGroup is null. This is bad. ");
            }
        }

        return perWalletModelDataToReturn;
    }

    /**
     * Create a new block store.
     * @param dateToReplayFrom
     * @return height tof new block chain after truncate.
     * @throws IOException
     * @throws BlockStoreException
     */
    public int createNewBlockStoreForReplay(Date dateToReplayFrom) throws IOException, BlockStoreException {
        log.debug("Loading/ creating blockstore ...");
        if (blockStore != null) {
            try {
                blockStore.close();
                blockStore = null;
            } catch (NullPointerException npe) {
                log.debug("NullPointerException on blockstore close");
            }
        }
        if (dateToReplayFrom != null) {
            blockStore = createBlockStore(dateToReplayFrom, true, true);
        } else {
            Date oneSecondAfterGenesis = new Date(MultiBitService.genesisBlockCreationDate.getTime() + 1000);
            blockStore = createBlockStore(oneSecondAfterGenesis, true, true);
        }
        log.debug("Blockstore is '" + blockStore + "'");

        log.debug("Creating blockchain ...");
        blockChain = new MultiBitBlockChain(this.bitcoinController.getModel().getNetworkParameters(), blockStore);
        log.debug("Created blockchain '" + blockChain + "'");

        // Hook up the wallets to the new blockchain.
        if (blockChain != null) {
            List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();
            for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                blockChain.addWallet(loopPerWalletModelData.getWallet());
            }
        }  
        return blockChain.getBestChainHeight();
    }

    /**
     * Download the block chain.
     */
    public void downloadBlockChain() {
        @SuppressWarnings("rawtypes")
        SwingWorker worker = new SwingWorker() {
            @Override
            protected Object doInBackground() throws Exception {
                logger.debug("Downloading blockchain");
                peerGroup.downloadBlockChain();
                return null; // return not used
            }
        };
        worker.execute();
    }

    /**
     * Send bitcoins from the active wallet.
     * 
     * @param sendAddressString
     *            the address to send to, as a String
     * @param amount
     *            the amount to send to, in BTC, as a String
     * @param fee
     *            fee to pay in nanocoin
     * 
     * @return The sent transaction (may be null if there were insufficient
     *         funds for send)
     * @throws EncrypterDecrypterException
     */

    public Transaction sendCoins(WalletData perWalletModelData, String sendAddressString, String amount, BigInteger fee,
            CharSequence password) throws java.io.IOException, AddressFormatException, KeyCrypterException {
        // Send the coins
        Address sendAddress = new Address(networkParameters, sendAddressString);

        log.debug("MultiBitService#sendCoins - Just about to send coins");
        KeyParameter aesKey = null;
        if (perWalletModelData.getWallet().getEncryptionType() != EncryptionType.UNENCRYPTED) {
            aesKey = perWalletModelData.getWallet().getKeyCrypter().deriveKey(password);
        }
        SendRequest request = SendRequest.to(sendAddress, Utils.toNanoCoins(amount));
        request.aesKey = aesKey;
        request.fee = fee;
        Wallet.SendResult sendResult = perWalletModelData.getWallet().sendCoins(peerGroup, request);
        log.debug("MultiBitService#sendCoins - Sent coins has completed");
        Transaction sendTransaction = sendResult.tx;

        assert sendTransaction != null;
        // We should never try to send more
        // coins than we have!
        // throw an exception if sendTransaction is null - no money.
        if (sendTransaction != null) {
            log.debug("MultiBitService#sendCoins - Sent coins. Transaction hash is {}", sendTransaction.getHashAsString());

            try {
                this.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (WalletSaveException wse) {
                log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
            } catch (WalletVersionException wse) {
                log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
            }

            try {
                // Notify other wallets of the send (it might be a send to or from them).
                List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();

                if (perWalletModelDataList != null) {
                    for (WalletData loopPerWalletModelData : perWalletModelDataList) {
                        if (!perWalletModelData.getWalletFilename().equals(loopPerWalletModelData.getWalletFilename())) {
                            Wallet loopWallet = loopPerWalletModelData.getWallet();
                            if (loopWallet.isPendingTransactionRelevant(sendTransaction)) {
                                // The loopPerWalletModelData is marked as dirty.
                                if (loopPerWalletModelData.getWalletInfo() != null) {
                                    synchronized (loopPerWalletModelData.getWalletInfo()) {
                                        loopPerWalletModelData.setDirty(true);
                                    }
                                } else {
                                    loopPerWalletModelData.setDirty(true);
                                }
                                if (loopWallet.getTransaction(sendTransaction.getHash()) == null) {
                                    log.debug("MultiBit adding a new pending transaction for the wallet '"
                                            + loopPerWalletModelData.getWalletDescription() + "'\n" + sendTransaction.toString());
                                    loopWallet.receivePending(sendTransaction, null);
                                }
                            }
                        }
                    }
                }
            } catch (ScriptException e) {
                e.printStackTrace();
            } catch (VerificationException e) {
                e.printStackTrace();
            }
        }
        return sendTransaction;
    }

    public PeerGroup getPeerGroup() {
        return peerGroup;
    }

    public MultiBitBlockChain getChain() {
        return blockChain;
    }

    public BlockStore getBlockStore() {
        return blockStore;
    }

    public SecureRandom getSecureRandom() {
        return secureRandom;
    };

    public String getCheckpointsFilename() {
        return checkpointsFilename;
    }

    public MultiBitCheckpointManager getCheckpointManager() {
        return checkpointManager;
    };
}
