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

import com.google.dogecoin.core.MultiBitBlockChain;
import com.google.dogecoin.core.*;
import com.google.dogecoin.core.Wallet.SendRequest;
import com.google.dogecoin.crypto.KeyCrypterException;
import com.google.dogecoin.discovery.DnsDiscovery;
import com.google.dogecoin.store.BlockStore;
import com.google.dogecoin.store.BlockStoreException;
import com.google.dogecoin.store.SPVBlockStore;
import com.google.common.util.concurrent.ListenableFuture;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandlerException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.model.core.StatusEnum;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.security.SecureRandom;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;


/**
 * <p>
 * MultiBitService encapsulates the interaction with the dogecoin netork
 * including: o Peers o Block chain download o sending / receiving bitcoins
 * <p/>
 * The testnet can be slow or flaky as it's a shared resource. You can use the
 * <a href="http://sourceforge
 * .net/projects/dogecoin/files/Bitcoin/testnet-in-a-box/">testnet in a box</a>
 * to do everything purely locally.
 * </p>
 */
public class MultiBitService {
  private static final String TESTNET3_GENESIS_HASH = "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943";

  private static final Logger log = LoggerFactory.getLogger(MultiBitService.class);

  public static final String MULTIBIT_PREFIX = "multidoge";
  public static final String TESTNET_PREFIX = "testnet";
  public static final String TESTNET3_PREFIX = "testnet3";
  public static final String SEPARATOR = "-";

  public static final String BLOCKCHAIN_SUFFIX = ".blockchain";
  public static final String SPV_BLOCKCHAIN_SUFFIX = ".spvchain";
  public static final String CHECKPOINTS_SUFFIX = ".checkpoints";
  public static final String WALLET_SUFFIX = ".wallet";

  public static final String IRC_CHANNEL_TEST = "#bitcoinTEST";
  public static final String IRC_CHANNEL_TESTNET3 = "#bitcoinTEST3";

  public Logger logger = LoggerFactory.getLogger(MultiBitService.class.getName());

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
      genesisBlockCreationDate = format.parse("2013-12-06 10:25:40");
    } catch (ParseException e) {
      // Will never happen.
      e.printStackTrace();
    }
  }

  /**
   * @param bitcoinController BitcoinController
   */
  public MultiBitService(BitcoinController bitcoinController) {
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
      blockStore = createBlockStore(null, false);
      log.debug("Blockstore is '" + blockStore + "'");

      log.debug("Creating blockchain ...");
      blockChain = new MultiBitBlockChain(networkParameters, blockStore);
      log.debug("Created blockchain '" + blockChain + "' with height " + blockChain.getBestChainHeight());

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
            new Object[]{blockchainFilename, e.getClass().getName() + " " + e.getMessage()})));
    log.error("Error creating MultiBitService " + e.getClass().getName() + " " + e.getMessage());
  }

  private BlockStore createBlockStore(Date checkpointDate, boolean createNew) throws BlockStoreException, IOException {
    BlockStore blockStore = null;

    String filePrefix = getFilePrefix();
    log.debug("filePrefix = " + filePrefix);

    if ("".equals(controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory())) {
      blockchainFilename = filePrefix + SPV_BLOCKCHAIN_SUFFIX;
      checkpointsFilename = filePrefix + CHECKPOINTS_SUFFIX;
    } else {
      blockchainFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
              + filePrefix + SPV_BLOCKCHAIN_SUFFIX;
      checkpointsFilename = controller.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator
              + filePrefix + CHECKPOINTS_SUFFIX;
    }

    File blockStoreFile = new File(blockchainFilename);
    boolean blockStoreCreatedNew = !blockStoreFile.exists();

    // Ensure there is a checkpoints file.
    File checkpointsFile = new File(checkpointsFilename);
    if (!checkpointsFile.exists()) {
      bitcoinController.getFileHandler().copyCheckpointsFromInstallationDirectory(checkpointsFilename);
    }

    // Use the larger of the installed checkpoints file and the user data checkpoint file (larger = more recent).
    ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();
    String installedCheckpointsFilename = applicationDataDirectoryLocator.getInstallationDirectory() + File.separator + MultiBitService.getFilePrefix() + MultiBitService.CHECKPOINTS_SUFFIX;
    log.debug("Installed checkpoints file = '" + installedCheckpointsFilename + "'.");

    File installedCheckpointsFile = new File(installedCheckpointsFilename);
    long sizeOfUserDataCheckpointsFile = 0;
    if (checkpointsFile.exists()) {
      sizeOfUserDataCheckpointsFile = checkpointsFile.length();
    }
    if (installedCheckpointsFile.exists() && installedCheckpointsFile.length() > sizeOfUserDataCheckpointsFile) {
      // The installed checkpoints file is longer (more checkpoints) so use that.
      checkpointsFilename = installedCheckpointsFilename;
      checkpointsFile = installedCheckpointsFile;
      log.debug("Using installed checkpoints file as it is longer than user data checkpoints - " + installedCheckpointsFile.length() + " bytes versus " + sizeOfUserDataCheckpointsFile + " bytes.");
    } else {
      log.debug("Using user data checkpoints file as it is longer/same size as installed checkpoints - " + sizeOfUserDataCheckpointsFile + " bytes versus " + installedCheckpointsFile.length() + " bytes.");
    }

    // If the spvBlockStore is to be created new
    // or its size is 0 bytes delete the file so that it is recreated fresh (fix for issue 165).
    if (createNew || blockStoreFile.length() == 0) {
      // Garbage collect any closed references to the blockchainFile.
      System.gc();
      blockStoreFile.setWritable(true);
      boolean deletedOk = blockStoreFile.delete();
      log.debug("Deleting SPV block store '{}' from disk.1", blockchainFilename + ", deletedOk = " + deletedOk);
      blockStoreCreatedNew = true;
    }

    log.debug("Opening / Creating SPV block store '{}' from disk", blockchainFilename);
    try {
      blockStore = new SPVBlockStore(networkParameters, blockStoreFile);
    } catch (BlockStoreException bse) {
      try {
        log.error("Failed to open/ create SPV block store '{}' from disk", blockchainFilename);
        // If the block store creation failed, delete the block store file and try again.

        // Garbage collect any closed references to the blockchainFile.
        System.gc();
        blockStoreFile.setWritable(true);
        boolean deletedOk = blockStoreFile.delete();
        log.debug("Deleting SPV block store '{}' from disk.2", blockchainFilename + ", deletedOk = " + deletedOk);
        blockStoreCreatedNew = true;

        blockStore = new SPVBlockStore(networkParameters, blockStoreFile);
      } catch (BlockStoreException bse2) {
        bse2.printStackTrace();
        log.error("Unrecoverable failure in opening block store. This is bad.");
        // Throw the exception so that it is indicated on the UI.
        throw bse2;
      }
    }

    // Load the existing checkpoint file and checkpoint from today.
    if (blockStore != null && checkpointsFile.exists()) {
      FileInputStream stream = null;
      try {
        stream = new FileInputStream(checkpointsFile);
        if (checkpointDate == null) {
          if (blockStoreCreatedNew) {
            // Brand new block store - checkpoint from today. This
            // will go back to the last checkpoint.
            CheckpointManager.checkpoint(networkParameters, stream, blockStore, (new Date()).getTime() / 1000);
          }
        } else {
          // Use checkpoint date (block replay).
          CheckpointManager.checkpoint(networkParameters, stream, blockStore, checkpointDate.getTime() / 1000);
        }
      } finally {
        if (stream != null) {
          stream.close();
          stream = null;
        }
      }
    }
    return blockStore;
  }

  public void createNewPeerGroup() {
    peerGroup = new MultiBitPeerGroup(bitcoinController, networkParameters, blockChain);
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
/*      // Use DNS for production, IRC for test.
      if (TESTNET3_GENESIS_HASH.equals(bitcoinController.getModel().getNetworkParameters().getGenesisBlock().getHashAsString())) {
        peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TESTNET3));
      } else if (NetworkParameters.testNet().equals(bitcoinController.getModel().getNetworkParameters())) {
        peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TEST));
      } else {
        peerGroup.addPeerDiscovery(new DnsDiscovery(networkParameters));
      }*/
        //DOGE: Only production for now.
        peerGroup.addPeerDiscovery(new DnsDiscovery(networkParameters));
    }
    // Add the controller as a PeerEventListener.
    peerGroup.addEventListener(bitcoinController.getPeerEventListener());

    // Add all existing wallets to the PeerGroup.
    if (controller != null && controller.getModel() != null) {
      List<WalletData> perWalletDataModels = bitcoinController.getModel().getPerWalletModelDataList();
      if (perWalletDataModels != null) {
        Iterator<WalletData> iterator = perWalletDataModels.iterator();
        if (iterator != null) {
          while (iterator.hasNext()) {
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
/*    BitcoinController bitcoinController = MultiBit.getBitcoinController();
    // testnet3
    if (TESTNET3_GENESIS_HASH.equals(bitcoinController.getModel().getNetworkParameters().getGenesisBlock().getHashAsString())) {
      return MULTIBIT_PREFIX + SEPARATOR + TESTNET3_PREFIX;
    } else if (NetworkParameters.testNet().equals(bitcoinController.getModel().getNetworkParameters())) {
      return MULTIBIT_PREFIX + SEPARATOR + TESTNET_PREFIX;
    } else {*/
      return MULTIBIT_PREFIX;
    //} //TODO DOGE: No testnet...
  }

  /**
   * Initialize wallet from the wallet filename.
   *
   * @param walletFilename
   * @return perWalletModelData
   */
  public WalletData addWalletFromFilename(String walletFilename) throws IOException {
    WalletData perWalletModelDataToReturn = null;
    Wallet wallet = null;

    File walletFile = null;
    boolean walletFileIsADirectory = false;
    boolean newWalletCreated = false;

    if (walletFilename != null) {
      walletFile = new File(walletFilename);
      if (walletFile.isDirectory()) {
        walletFileIsADirectory = true;
      } else {

        perWalletModelDataToReturn = bitcoinController.getFileHandler().loadFromFile(walletFile);
        if (perWalletModelDataToReturn != null) {
          wallet = perWalletModelDataToReturn.getWallet();
        }

      }
    }

    if (walletFilename == null || walletFilename.equals("") || walletFileIsADirectory) {
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
        perWalletModelDataToReturn = bitcoinController.getFileHandler().loadFromFile(walletFile);
        if (perWalletModelDataToReturn != null) {
          wallet = perWalletModelDataToReturn.getWallet();
          newWalletCreated = true;
        }
      } else {
        // Create a brand new wallet - by default unencrypted.
        wallet = new Wallet(networkParameters);
        ECKey newKey = new ECKey();
        wallet.addKey(newKey);

        perWalletModelDataToReturn = bitcoinController.getModel().addWallet(bitcoinController, wallet, walletFile.getAbsolutePath());

        // Create a wallet info.
        WalletInfoData walletInfo = new WalletInfoData(walletFile.getAbsolutePath(), wallet, MultiBitWalletVersion.PROTOBUF);
        perWalletModelDataToReturn.setWalletInfo(walletInfo);

        // Set a default description.
        String defaultDescription = controller.getLocaliser().getString("createNewWalletSubmitAction.defaultDescription");
        perWalletModelDataToReturn.setWalletDescription(defaultDescription);

        try {
          bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelDataToReturn, true);

          newWalletCreated = true;

          // Backup the wallet and wallet info.
          BackupManager.INSTANCE.backupPerWalletModelData(bitcoinController.getFileHandler(), perWalletModelDataToReturn);

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
      List<ECKey> keys = wallet.getKeychain();
      if (keys != null) {
        if (!newWalletCreated) {
          perWalletModelDataToReturn = bitcoinController.getModel().getPerWalletModelDataByWalletFilename(walletFilename);
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
        peerGroup.addEventListener(bitcoinController.getPeerEventListener());
      } else {
        log.error("Could not add wallet '" + walletFilename + "' to the peerGroup as the peerGroup is null. This is bad. ");
      }

    }

    return perWalletModelDataToReturn;
  }

  /**
   * Create a new block store.
   *
   * @param dateToReplayFrom The date to start the replay task from
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

    // The CheckpointManager removes a week to cater for block header drift.
    // Any date before genesis + 1 week gets adjusted accordingly.
    Date genesisPlusOnwWeekAndASecond = new Date(MultiBitService.genesisBlockCreationDate.getTime() + (86400 * 7 + 1) * 1000);

    if (dateToReplayFrom != null) {
      if (dateToReplayFrom.getTime() < genesisPlusOnwWeekAndASecond.getTime()) {
        dateToReplayFrom = genesisPlusOnwWeekAndASecond;
      }
      blockStore = createBlockStore(dateToReplayFrom, true);
    } else {
      blockStore = createBlockStore(genesisPlusOnwWeekAndASecond, true);
    }
    log.debug("Blockstore is '" + blockStore + "'");

    log.debug("Creating blockchain ...");
    blockChain = new MultiBitBlockChain(bitcoinController.getModel().getNetworkParameters(), blockStore);
    log.debug("Created blockchain '" + blockChain + "'");

    // Hook up the wallets to the new blockchain.
    if (blockChain != null) {
      List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
      for (WalletData loopPerWalletModelData : perWalletModelDataList) {
        if (loopPerWalletModelData.getWallet() != null) {
          blockChain.addWallet(loopPerWalletModelData.getWallet());
        }
      }
    }
    return blockChain.getBestChainHeight();
  }

  /**
   * Send bitcoins from the active wallet.
   *
   * @return The sent transaction (may be null if there were insufficient
   *         funds for send)
   * @throws KeyCrypterException
   * @throws IOException
   * @throws AddressFormatException
   */

  public Transaction sendCoins(WalletData perWalletModelData, SendRequest sendRequest,
                               CharSequence password) throws java.io.IOException, AddressFormatException, KeyCrypterException {

    // Ping the peers to check the dogecoin network connection
    List<Peer> connectedPeers = peerGroup.getConnectedPeers();
    boolean atLeastOnePingWorked = false;
    if (connectedPeers != null) {
      for (Peer peer : connectedPeers) {

        log.debug("Ping: {}", peer.getAddress().toString());

        try {
          ListenableFuture<Long> result = peer.ping();
          result.get(4, TimeUnit.SECONDS);
          atLeastOnePingWorked = true;
          break;
        } catch (ProtocolException e) {
          log.warn("Peer '" + peer.getAddress().toString() + "' failed ping test. Message was " + e.getMessage());
        } catch (InterruptedException e) {
          log.warn("Peer '" + peer.getAddress().toString() + "' failed ping test. Message was " + e.getMessage());
        } catch (ExecutionException e) {
          log.warn("Peer '" + peer.getAddress().toString() + "' failed ping test. Message was " + e.getMessage());
        } catch (TimeoutException e) {
          log.warn("Peer '" + peer.getAddress().toString() + "' failed ping test. Message was " + e.getMessage());
        }
      }
    }

    if (!atLeastOnePingWorked) {
      throw new IllegalStateException("All peers failed ping test (check network)");
    }

    // Send the coins

    log.debug("MultiBitService#sendCoins - Just about to send coins");
    KeyParameter aesKey = null;
    if (perWalletModelData.getWallet().getEncryptionType() != EncryptionType.UNENCRYPTED) {
      aesKey = perWalletModelData.getWallet().getKeyCrypter().deriveKey(password);
    }
    sendRequest.aesKey = aesKey;
    sendRequest.fee = BigInteger.ZERO;
    sendRequest.feePerKb = BitcoinModel.SEND_FEE_PER_KB_DEFAULT;

    sendRequest.tx.getConfidence().addEventListener(perWalletModelData.getWallet().getTxConfidenceListener());

    try {
      // The transaction is already added to the wallet (in SendBitcoinConfirmAction) so here we just need
      // to sign it, commit it and broadcast it.
      perWalletModelData.getWallet().sign(sendRequest);
      perWalletModelData.getWallet().commitTx(sendRequest.tx);

      // The tx has been committed to the pending pool by this point (via sendCoinsOffline -> commitTx), so it has
      // a txConfidenceListener registered. Once the tx is broadcast the peers will update the memory pool with the
      // count of seen peers, the memory pool will update the transaction confidence object, that will invoke the
      // txConfidenceListener which will in turn invoke the wallets event listener onTransactionConfidenceChanged
      // method.
      peerGroup.broadcastTransaction(sendRequest.tx);

      log.debug("Sending transaction '" + Utils.bytesToHexString(sendRequest.tx.bitcoinSerialize()) + "'");
    } catch (VerificationException e1) {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }

    Transaction sendTransaction = sendRequest.tx;

    log.debug("MultiBitService#sendCoins - Sent coins has completed");

    assert sendTransaction != null;
    // We should never try to send more coins than we have!
    // throw an exception if sendTransaction is null - no money.
    if (sendTransaction != null) {
      log.debug("MultiBitService#sendCoins - Sent coins. Transaction hash is {}", sendTransaction.getHashAsString() + ", identityHashcode = " + System.identityHashCode(sendTransaction));

      if (sendTransaction.getConfidence() != null) {
        log.debug("Added bitcoinController " + System.identityHashCode(bitcoinController) + " as listener to tx = " + sendTransaction.getHashAsString());
        sendTransaction.getConfidence().addEventListener(bitcoinController);
      } else {
        log.debug("Cannot add bitcoinController as listener to tx = " + sendTransaction.getHashAsString() + " no transactionConfidence");
      }

      try {
        bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, false);
      } catch (WalletSaveException wse) {
        log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
        MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
      } catch (WalletVersionException wse) {
        log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
        MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
      }

      try {
        // Notify other wallets of the send (it might be a send to or from them).
        List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();

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
  }

  ;

  public String getCheckpointsFilename() {
    return checkpointsFilename;
  }

  public MultiBitCheckpointManager getCheckpointManager() {
    return checkpointManager;
  }
}