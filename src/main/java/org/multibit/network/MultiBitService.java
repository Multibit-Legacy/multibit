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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.SimpleTimeZone;
import java.util.Stack;

import javax.swing.SwingWorker;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandlerException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.StatusEnum;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;
import org.multibit.store.ReplayableBlockStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Block;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.MultiBitBlockChain;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerAddress;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.ProtocolException;
import com.google.bitcoin.core.Sha256Hash;
import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.discovery.DnsDiscovery;
import com.google.bitcoin.discovery.IrcDiscovery;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;

/**
 * <p>
 * MultiBitService encapsulates the interaction with the bitcoin netork
 * including: o Peers o Block chain download o sending / receiving bitcoins
 * 
 * It is based on the bitcoinj PingService code
 * 
 * The testnet can be slow or flaky as it's a shared resource. You can use the
 * <a href="http://sourceforge
 * .net/projects/bitcoin/files/Bitcoin/testnet-in-a-box/">testnet in a box</a>
 * to do everything purely locally.
 * </p>
 */
public class MultiBitService {
    private static final Logger log = LoggerFactory.getLogger(MultiBitService.class);

    private static final int NUMBER_OF_MILLISECOND_IN_A_SECOND = 1000;
    private static final int MAXIMUM_EXPECTED_LENGTH_OF_ALTERNATE_CHAIN = 6;

    public static final String MULTIBIT_PREFIX = "multibit";
    public static final String TEST_NET_PREFIX = "testnet";
    public static final String SEPARATOR = "-";

    public static final String BLOCKCHAIN_SUFFIX = ".blockchain";
    public static final String WALLET_SUFFIX = ".wallet";

    public static final String IRC_CHANNEL_TEST = "#bitcoinTEST";;

    static boolean restartListenerHasBeenAddedToPeerGroup = false;

    public Logger logger = LoggerFactory.getLogger(MultiBitService.class.getName());

    private Wallet wallet;

    private MultiBitPeerGroup peerGroup;

    private String blockchainFilename;

    private MultiBitBlockChain blockChain;

    private ReplayableBlockStore blockStore;

    private boolean useTestNet;

    private MultiBitController controller;

    private final NetworkParameters networkParameters;

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
     * @param useTestNet
     *            true = test net, false = production
     * @param controller
     *            MutliBitController
     */
    public MultiBitService(boolean useTestNet, MultiBitController controller) {
        this(useTestNet, controller.getModel().getUserPreference(MultiBitModel.WALLET_FILENAME), controller);
    }

    /**
     * 
     * @param useTestNet
     *            true = test net, false = production
     * @param walletFilename
     *            filename of current wallet
     * @param controller
     *            MutliBitController
     */
    public MultiBitService(boolean useTestNet, String walletFilename, MultiBitController controller) {
        this.useTestNet = useTestNet;
        this.controller = controller;

        networkParameters = useTestNet ? NetworkParameters.testNet() : NetworkParameters.prodNet();

        try {
            // Load the block chain.
            String filePrefix = getFilePrefix();
            if ("".equals(controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
                blockchainFilename = filePrefix + BLOCKCHAIN_SUFFIX;
            } else {
                blockchainFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
                        + filePrefix + BLOCKCHAIN_SUFFIX;
            }

            // Check to see if the user has a blockchain and copy over the
            // installed one if they do not.
            controller.getFileHandler().copyBlockChainFromInstallationDirectory(this, blockchainFilename);

            log.debug("Reading block store '{}' from disk", blockchainFilename);

            blockStore = new ReplayableBlockStore(networkParameters, new File(blockchainFilename), false);

            log.debug("Creating blockchain ...");
            blockChain = new MultiBitBlockChain(networkParameters, blockStore);
            log.debug("Created blockchain '" + blockChain + "'");

            log.debug("Creating peergroup ...");
            peerGroup = createNewPeerGroup();
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
    }
    
    private void handleError(Exception e) {
        controller.setOnlineStatus(StatusEnum.ERROR);
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitService.couldNotLoadBlockchain", 
                new Object[]{blockchainFilename, e.getClass().getName() + " " + e.getMessage()})));
        log.error("Error creating MultiBitService " + e.getClass().getName() + " " + e.getMessage());        
    }

    private MultiBitPeerGroup createNewPeerGroup() {
        MultiBitPeerGroup peerGroup = new MultiBitPeerGroup(controller, networkParameters, blockChain);
        peerGroup.setFastCatchupTimeSecs(0); // genesis block
        peerGroup.setUserAgent("MultiBit", controller.getLocaliser().getVersionNumber());

        String singleNodeConnection = controller.getModel().getUserPreference(MultiBitModel.SINGLE_NODE_CONNECTION);
        if (singleNodeConnection != null && !singleNodeConnection.equals("")) {
            try {
                peerGroup.addAddress(new PeerAddress(InetAddress.getByName(singleNodeConnection)));
                peerGroup.setMaxConnections(1);
            } catch (UnknownHostException e) {
                log.error(e.getMessage(), e);
            }
        } else {
            // Use DNS for production, IRC for test.
            if (useTestNet) {
                peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TEST));
            } else {
                peerGroup.addPeerDiscovery(new DnsDiscovery(networkParameters));
            }
        }
        // Add the controller as a PeerEventListener.
        peerGroup.addEventListener(controller);
        return peerGroup;
    }

    public String getFilePrefix() {
        return useTestNet ? MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX : MULTIBIT_PREFIX;
    }

    /**
     * Initialize wallet from the wallet filename.
     * 
     * @param walletFilename
     * @return perWalletModelData
     */
    public PerWalletModelData addWalletFromFilename(String walletFilename) throws IOException {
        PerWalletModelData perWalletModelDataToReturn = null;

        File walletFile = null;
        boolean walletFileIsADirectory = false;
        boolean newWalletCreated = false;

        if (walletFilename != null) {
            walletFile = new File(walletFilename);
            if (walletFile.isDirectory()) {
                walletFileIsADirectory = true;
            } else {

                perWalletModelDataToReturn = controller.getFileHandler().loadFromFile(walletFile);
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
                perWalletModelDataToReturn = controller.getFileHandler().loadFromFile(walletFile);
                if (perWalletModelDataToReturn != null) {
                    wallet = perWalletModelDataToReturn.getWallet();
                }

                newWalletCreated = true;
            } else {
                // Create a brand new wallet - by default protobuf.
                wallet = new Wallet(networkParameters);
                ECKey newKey = new ECKey();
                wallet.keychain.add(newKey);

                perWalletModelDataToReturn = controller.getModel().addWallet(wallet, walletFile.getAbsolutePath());

                // Create a wallet info.
                WalletInfo walletInfo = new WalletInfo(walletFile.getAbsolutePath(), WalletVersion.PROTOBUF);
                perWalletModelDataToReturn.setWalletInfo(walletInfo);

                // Set a default description.
                String defaultDescription = controller.getLocaliser().getString("createNewWalletSubmitAction.defaultDescription");
                perWalletModelDataToReturn.setWalletDescription(defaultDescription);

                try {
                    controller.getFileHandler().savePerWalletModelData(perWalletModelDataToReturn, true);

                    newWalletCreated = true;
                } catch (WalletSaveException wse) {
                    log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                    MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
                }
            }
        }

        if (wallet != null) {
            // Add the keys for this wallet to the address book as receiving addresses.
            ArrayList<ECKey> keys = wallet.keychain;
            if (keys != null) {
                if (!newWalletCreated) {
                    perWalletModelDataToReturn = controller.getModel().getPerWalletModelDataByWalletFilename(walletFilename);
                }
                if (perWalletModelDataToReturn != null) {
                    WalletInfo walletInfo = perWalletModelDataToReturn.getWalletInfo();
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

            // Add wallet to PeerGroup - this is done in a background thread as it is slow.
            @SuppressWarnings("rawtypes")
            SwingWorker worker = new SwingWorker() {
                @Override
                protected Object doInBackground() throws Exception {
                    peerGroup.addWallet(wallet);
                    return null; // not used
                }
            };
            worker.execute();
        }

        return perWalletModelDataToReturn;
    }

    /**
     * Replay blockchain.
     * 
     * @param dateToReplayFrom
     *            the date on the blockchain to replay from - if missing replay
     *            from genesis block
     */
    public void replayBlockChain(Date dateToReplayFrom) throws BlockStoreException {
        // Navigate backwards in the blockchain to work out how far back in
        // time to go.
        log.debug("Starting replay of blockchain from date = '" + dateToReplayFrom + "'");

        StoredBlock storedBlock = null;
        
        Stack<StoredBlock> blockStack = new Stack<StoredBlock>();
        if (dateToReplayFrom == null || genesisBlockCreationDate.after(dateToReplayFrom)) {
            // create empty new block store
            if (blockStore != null) {
                blockStore.close();
            }
            blockStore = new ReplayableBlockStore(networkParameters, new File(blockchainFilename), true);

            log.debug("Creating new blockStore.2 - need to redownload from Genesis block");
            blockChain = new MultiBitBlockChain(networkParameters, (BlockStore) blockStore);
            log.debug("Created new blockStore.2 '" + blockChain + "'");
        } else {
            storedBlock = blockStore.getChainHead();

            assert storedBlock != null;

            boolean haveGoneBackInTimeEnough = false;
            int numberOfBlocksGoneBackward = 0;
            blockStack.push(storedBlock);

            while (!haveGoneBackInTimeEnough) {
                if (storedBlock == null) {
                    // Null result of previous get previous - will have to stop
                    // navigating backwards.
                    break;
                }
                Block header = storedBlock.getHeader();
                if (header == null) {
                    log.debug("No header for stored block " + storedBlock.getHeight());
                    break;
                }

                long headerTimeInSeconds = header.getTimeSeconds();
                if (headerTimeInSeconds < (dateToReplayFrom.getTime() / NUMBER_OF_MILLISECOND_IN_A_SECOND)) {
                    haveGoneBackInTimeEnough = true;
                } else {
                    try {
                        StoredBlock previousBlock = storedBlock.getPrev(blockStore);
                        if (previousBlock == null) {
                            log.debug("Could not navigate backwards form storedBlock " + storedBlock.getHeight());
                            break;
                        } else {
                            storedBlock = previousBlock;
                            blockStack.push(storedBlock);
                        }
                        numberOfBlocksGoneBackward++;
                    } catch (BlockStoreException e) {
                        e.printStackTrace();
                        // we have to stop navigating backwards
                        break;
                    }
                }
            }

            // In case the chain head was on an alternate fork go back more
            // blocks to ensure back on the main chain.
            while (numberOfBlocksGoneBackward < MAXIMUM_EXPECTED_LENGTH_OF_ALTERNATE_CHAIN) {
                try {
                    StoredBlock previousBlock = storedBlock.getPrev(blockStore);
                    if (previousBlock == null) {
                        log.debug("Could not navigate backwards form storedBlock " + storedBlock.getHeight());
                        break;
                    } else {
                        storedBlock = previousBlock;
                        blockStack.push(storedBlock);
                    }
                    numberOfBlocksGoneBackward++;
                } catch (BlockStoreException e) {
                    e.printStackTrace();
                    // We have to stop - fail.
                    break;
                }
            }
          
            // If we have the blocks cached then we do not need to redownload them
            String applicationDataDirectory = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory();
            String cacheDirectoryString = applicationDataDirectory + File.separator + MultiBitController.CACHE_DIRECTORY;
            File cacheDirectory = new File(cacheDirectoryString);
            if (!cacheDirectory.exists()) {
                cacheDirectory.mkdir();
            }
            
            // See if block is already cached.
            String blockFilename = cacheDirectory + File.separator + storedBlock.getHeader().getHashAsString() + MultiBitController.BLOCK_SUFFIX;
            File blockFile = new File(blockFilename);
                
            if (blockFile.exists()) {
                log.debug("Can replay blocks from the cache from storedBlock height = " + storedBlock.getHeight());
                storedBlock = replayFromBlockCache(blockStack);
            }   
            

            // Set the block chain head to the block just before the
            // earliest transaction in the wallet.
            blockChain.setChainHeadClearCachesAndTruncateBlockStore(storedBlock);
        }

        // Restart peerGroup and download.
        Message message = new Message(controller.getLocaliser().getString("multiBitService.stoppingBitcoinNetworkConnection"), false);
        MessageManager.INSTANCE.addMessage(message);
        peerGroup.stop();

        // Reset UI to zero peers.
        controller.onPeerDisconnected(null, 0);

        if (dateToReplayFrom != null && storedBlock != null) {
            message = new Message(controller.getLocaliser().getString(
                    "resetTransactionSubmitAction.replayingBlockchain",
                    new Object[] { DateFormat.getDateInstance(DateFormat.MEDIUM, controller.getLocaliser().getLocale()).format(
                            storedBlock.getHeader().getTime()) }), false);
        } else {
            message = new Message(controller.getLocaliser().getString(
                    "resetTransactionSubmitAction.replayingBlockchain",
                    new Object[] { DateFormat.getDateInstance(DateFormat.MEDIUM, controller.getLocaliser().getLocale()).format(
                            genesisBlockCreationDate) }), false);
        }
        MessageManager.INSTANCE.addMessage(message);

        peerGroup = createNewPeerGroup();
        peerGroup.start();
 
        downloadBlockChain();
    }
    
    /**
     * Replay from block cache.
     * Replay the blocks in the stack given from the block cache.
     * If any blocks are not cached then return that block as the new block hash to be the truncated block chain head
     */
    private StoredBlock replayFromBlockCache(Stack<StoredBlock> blockStack) {
        log.debug("Replaying a stack of " + blockStack.size() + " from the block cache.");
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitService.replayingFromBlockCache")));
        StoredBlock newChainHead = blockStack.peek();
        
        String applicationDataDirectory = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory();
        String cacheDirectoryString = applicationDataDirectory + File.separator + MultiBitController.CACHE_DIRECTORY;
        File cacheDirectory = new File(cacheDirectoryString);
        if (!cacheDirectory.exists()) {
            cacheDirectory.mkdir();
        }
        
        while (!blockStack.isEmpty()) {
            newChainHead = blockStack.pop();
              
            // See if block is already cached.
            String blockFilename = cacheDirectory + File.separator + newChainHead.getHeader().getHashAsString() + MultiBitController.BLOCK_SUFFIX;
            File blockFile = new File(blockFilename);
            
            if (blockFile.exists()) {
                byte[] blockBytes;
                try {
                    blockBytes = getBytesFromFile(blockFile);
                    Block replayBlock = new Block(networkParameters, blockBytes);
                    // Replay the block.
                    controller.onBlock(newChainHead, replayBlock);
                    controller.onBlocksDownloaded(null, replayBlock, -1);
                } catch (IOException e) {
                    // We did not successfully read the block so start downloading from here.
                    return newChainHead;
                } catch (ProtocolException e) {
                    // We did not successfully read the block so start downloading from here.
                    return newChainHead;
                }
            } else {
                // The block is not cached so start downloading.
                return newChainHead;
            }
        }
        
        return newChainHead;
    }

    /**
     * download the block chain
     */
    public void downloadBlockChain() {
        @SuppressWarnings("rawtypes")
        SwingWorker worker = new SwingWorker() {
            @Override
            protected Object doInBackground() throws Exception {
                logger.debug("Downloading blockchain");
                peerGroup.downloadBlockChain();
                return null; // Return not used.
            }
        };
        worker.execute();
    }

    /**
     * send bitcoins from the active wallet
     * 
     * @param sendAddressString
     *            the address to send to, as a String
     * @param fee
     *            fee to pay in nanocoin
     * @param amount
     *            the amount to send to, in BTC, as a String
     * @return The sent transaction (may be null if there were insufficient
     *         funds for send)
     */

    public Transaction sendCoins(PerWalletModelData perWalletModelData, String sendAddressString, String amount, BigInteger fee)
            throws java.io.IOException, AddressFormatException {
        // Send the coins.
        Address sendAddress = new Address(networkParameters, sendAddressString);

        log.debug("MultiBitService#sendCoins - Just about to send coins");
        Transaction sendTransaction = perWalletModelData.getWallet().sendCoinsAsync(peerGroup, sendAddress,
                Utils.toNanoCoins(amount), fee);
        log.debug("MultiBitService#sendCoins - Sent coins has completed");

        assert sendTransaction != null; 
        // We should never try to send more coins than we have!
        // TODO: Throw an exception if sendTransaction is null - no money.
        if (sendTransaction != null) {
            log.debug("MultiBitService#sendCoins - Sent coins. Transaction hash is {}", sendTransaction.getHashAsString());

            try {
                controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (WalletSaveException wse) {
                log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
            }
            
            // clone the sent transaction
            try {
                Transaction clonedSentTransaction = new Transaction(networkParameters, sendTransaction.bitcoinSerialize());
                // Modify the transaction so that its TransactionOutputs are unspent.
                // What is spent from the perspective of the sender is available to the recipient.
                clonedSentTransaction.markOutputsAsSpendable();

                // Notify other wallets of the send (it might be a send to them).
                List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

                if (perWalletModelDataList != null) {
                    for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                        if (!perWalletModelData.getWalletFilename().equals(loopPerWalletModelData.getWalletFilename())) {
                            controller.onCoinsReceived(loopPerWalletModelData.getWallet(), clonedSentTransaction, null, null);
                        }
                    }
                }
            } catch (ProtocolException e) {
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

    public NetworkParameters getNetworkParameters() {
        return networkParameters;
    }

    public ReplayableBlockStore getBlockStore() {
        return blockStore;
    }
    
    private byte[] getBytesFromFile(File file) throws IOException {
        InputStream is = new FileInputStream(file);

        // Get the size of the file
        long length = file.length();

        // You cannot create an array using a long type.
        // It needs to be an int type.
        // Before converting to an int type, check
        // to ensure that file is not larger than Integer.MAX_VALUE.
        if (length > Integer.MAX_VALUE) {
            // File is too large
        }

        // Create the byte array to hold the data
        byte[] bytes = new byte[(int)length];

        // Read in the bytes
        int offset = 0;
        int numRead = 0;
        while (offset < bytes.length
               && (numRead=is.read(bytes, offset, bytes.length-offset)) >= 0) {
            offset += numRead;
        }

        // Ensure all the bytes have been read in
        if (offset < bytes.length) {
            throw new IOException("Could not completely read file "+file.getName());
        }

        // Close the input stream and return bytes
        is.close();
        return bytes;
    }
}
