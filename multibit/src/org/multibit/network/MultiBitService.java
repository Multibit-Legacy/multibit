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
import java.math.BigInteger;
import java.util.ArrayList;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBook;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.discovery.DnsDiscovery;
import com.google.bitcoin.discovery.IrcDiscovery;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;
import com.google.bitcoin.store.BoundedOverheadBlockStore;

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
    public static final String MULTIBIT_PREFIX = "multibit";
    public static final String TEST_NET_PREFIX = "testnet";
    public static final String SEPARATOR = "-";

    public static final String BLOCKCHAIN_SUFFIX = ".blockchain";
    public static final String WALLET_SUFFIX = ".wallet";

    public static final String IRC_CHANNEL_TEST = "#bitcoinTEST";;

    private Wallet wallet;

    private PeerGroup peerGroup;

    private BlockChain chain;

    private boolean useTestNet;

    private MultiBitController controller;

    private final NetworkParameters networkParameters;
    
    private FileHandler fileHandler;

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
        String filePrefix = useTestNet ? MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX : MULTIBIT_PREFIX;

        fileHandler = new FileHandler(controller);
        File walletFile;
        if (walletFilename != null) {
            walletFile = new File(walletFilename);
            wallet = fileHandler.loadWalletFromFile(walletFile);
        }

        if (wallet == null || walletFilename == null) {
            // use default wallet name - create if does not exist
            walletFilename = filePrefix + WALLET_SUFFIX;
            walletFile = new File(walletFilename);

            if (walletFile.exists()) {
                // wallet file exists with default name
                wallet = fileHandler.loadWalletFromFile(walletFile);
            } else {
                wallet = new Wallet(networkParameters);
                ECKey newKey = new ECKey();
                wallet.keychain.add(newKey);
                
                fileHandler.saveWalletToFile(wallet, walletFile);
            }
        }

        // add the keys for this wallet to the address book as receiving
        // addresses
        ArrayList<ECKey> keys = wallet.keychain;
        if (keys != null) {
            AddressBook addressBook = controller.getModel().getAddressBook();
            if (addressBook != null) {
                for (ECKey key : keys) {
                    if (key != null) {
                        Address address = key.toAddress(networkParameters);
                        addressBook.addReceivingAddressOfKey(address);
                    }
                }
            }
        }

        // Load the block chain, if there is one stored locally.
        System.out.println("Reading block store from disk");
        BlockStore blockStore;
        try {
            blockStore = new BoundedOverheadBlockStore(networkParameters, new File(filePrefix + ".blockchain"));

            // Connect to the localhost node. One minute timeout since we won't
            // try any other peers
            System.out.println("Connecting ...");
            chain = new BlockChain(networkParameters, wallet, blockStore);

            peerGroup = new MultiBitPeerGroup(controller, blockStore, networkParameters, chain, wallet);

            // use DNS for production, IRC for test
            if (useTestNet) {
                peerGroup.addPeerDiscovery(new IrcDiscovery(IRC_CHANNEL_TEST));
            } else {
                peerGroup.addPeerDiscovery(new DnsDiscovery(networkParameters));
            }
            // add the controller as a PeerEventListener
            peerGroup.addEventListener(controller);
            peerGroup.start();
        } catch (BlockStoreException e) {
            controller.displayMessage("multiBitService.errorText",
                    new Object[] { e.getClass().getName() + " " + e.getMessage() }, "multiBitService.errorTitleText");
        } catch (Exception e) {
            controller.displayMessage("multiBitService.errorText",
                    new Object[] { e.getClass().getName() + " " + e.getMessage() }, "multiBitService.errorTitleText");
        }
    }

    /**
     * download the block chain
     */
    public void downloadBlockChain() {
        peerGroup.downloadBlockChain();
    }

    /**
     * send bitcoins
     * 
     * @param sendAddressString
     *            the address to send to, as a String
     * @param fee
     *            fee to pay in nanocoin
     * @param amount
     *            the amount to send to, in BTC, as a String
     */

    public Transaction sendCoins(String sendAddressString, String amount, BigInteger fee) throws java.io.IOException,
            AddressFormatException {
        // send the coins
        Address sendAddress = new Address(networkParameters, sendAddressString);
        Transaction sendTransaction = wallet.sendCoins(peerGroup, sendAddress, Utils.toNanoCoins(amount), fee);
        assert sendTransaction != null; // We should never try to send more
        // coins than we have!
        // throw an exception if sendTransaction is null - no money
        if (sendTransaction != null) {
            System.out.println("MultiBitService#sendCoins - Sent coins. Transaction hash is "
                    + sendTransaction.getHashAsString());
            fileHandler.saveWalletToFile(wallet, new File(controller.getModel().getWalletFilename()));
        }
        return sendTransaction;
    }

    public PeerGroup getPeerGroup() {
        return peerGroup;
    }

    public BlockChain getChain() {
        return chain;
    }

    public NetworkParameters getNetworkParameters() {
        return networkParameters;
    }

    public boolean isUseTestNet() {
        return useTestNet;
    }
}
