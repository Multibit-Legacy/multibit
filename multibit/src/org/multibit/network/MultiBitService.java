/**
 * Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.multibit.network;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.BlockChain;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.PeerAddress;
import com.google.bitcoin.core.PeerGroup;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletEventListener;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.BlockStoreException;
import com.google.bitcoin.store.BoundedOverheadBlockStore;

/**
 * <p>
 * BitcoinService encapsulates the interaction with the bitcoin netork
 * including: o Peers o Block chain download o sending / receiving bitcoins
 * 
 * It is based on the bitcoinj PingService code
 * 
 * <p>
 * If running on TestNet (slow but better than using real coins on prodnet) do
 * the following:
 * <ol>
 * <li>Backup your current wallet.dat in case of unforeseen problems</li>
 * <li>Start your bitcoin client in test mode <code>bitcoin -testnet</code>.
 * This will create a new sub-directory called testnet and should not interfere
 * with normal wallets or operations.</li>
 * <li>(Optional) Choose a fresh address</li>
 * <li>(Optional) Visit the Testnet faucet
 * (https://testnet.freebitcoins.appspot.com/) to load your client with test
 * coins</li>
 * <li>Run <code>PingService -testnet</code></li>
 * <li>Wait for the block chain to download</li>
 * <li>Send some coins from your bitcoin client to the address provided in the
 * PingService console</li>
 * <li>Leave it running until you get the coins back again</li>
 * </ol>
 * </p>
 * 
 * <p>
 * The testnet can be slow or flaky as it's a shared resource. You can use the
 * <a href="http://sourceforge
 * .net/projects/bitcoin/files/Bitcoin/testnet-in-a-box/">testnet in a box</a>
 * to do everything purely locally.
 * </p>
 */
public class MultiBitService {
    public static final String MULTIBIT_PREFIX = "multibit";
    public static final String PRODUCTION_NET_PREFIX = "prodnet";
    public static final String TEST_NET_PREFIX = "testnet";
    public static final String SEPARATOR = "-";

    public static final String BLOCKCHAIN_SUFFIX = ".blockchain";
    public static final String WALLET_SUFFIX = ".wallet";

    private String walletFilename;
    private Wallet wallet;

    private PeerGroup peerGroup;

    /**
     * 
     * @param isProduction
     *            true = production, false = testnet
     * @param wallet
     *            current wallet
     */
    public MultiBitService(boolean useTestNet) {
        final NetworkParameters params = useTestNet ? NetworkParameters.testNet()
                : NetworkParameters.prodNet();
        String filePrefix = useTestNet ? MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX
                : MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX;

        // Try to read the wallet from storage, create a new one if not
        // possible.
        walletFilename = filePrefix + WALLET_SUFFIX;
        final File walletFile = new File(walletFilename);
        try {
            wallet = Wallet.loadFromFile(walletFile);
        } catch (IOException e) {
            wallet = new Wallet(params);
            wallet.keychain.add(new ECKey());
            try {
                wallet.saveToFile(walletFile);
            } catch (IOException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }
        }

        // Fetch the first key in the wallet (should be the only key).
        ECKey key = wallet.keychain.get(0);

        System.out.println(wallet);

        // Load the block chain, if there is one stored locally.
        System.out.println("Reading block store from disk");
        BlockStore blockStore;
        try {
            blockStore = new BoundedOverheadBlockStore(params, new File(filePrefix + ".blockchain"));

            // Connect to the localhost node. One minute timeout since we won't
            // try any other peers
            System.out.println("Connecting ...");
            BlockChain chain = new BlockChain(params, wallet, blockStore);

            peerGroup = new PeerGroup(blockStore, params, chain);
            // peerGroup.addAddress(new PeerAddress(InetAddress.getLocalHost(),
            // 8333));
            peerGroup.addAddress(new PeerAddress(InetAddress.getByName("98.143.152.19"), 8333));
            peerGroup.start();

            System.out.println("Send coins to: " + key.toAddress(params).toString());
            System.out.println("Waiting for coins to arrive. Press Ctrl-C to quit.");

            // The PeerGroup thread keeps us alive until something kills the
            // process.
        } catch (BlockStoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * download the block chain
     */
    public void downloadBlockChain() {
        peerGroup.downloadBlockChain();
    }

    /**
     * example code from PingService
     * 
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        boolean testNet = args.length > 0 && args[0].equalsIgnoreCase(TEST_NET_PREFIX);
        final NetworkParameters params = testNet ? NetworkParameters.testNet() : NetworkParameters
                .prodNet();
        String filePrefix = testNet ? MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX
                : MULTIBIT_PREFIX + SEPARATOR + TEST_NET_PREFIX;

        // Try to read the wallet from storage, create a new one if not
        // possible.
        Wallet wallet;
        final File walletFile = new File(filePrefix + WALLET_SUFFIX);
        try {
            wallet = Wallet.loadFromFile(walletFile);
        } catch (IOException e) {
            wallet = new Wallet(params);
            wallet.keychain.add(new ECKey());
            wallet.saveToFile(walletFile);
        }
        // Fetch the first key in the wallet (should be the only key).
        ECKey key = wallet.keychain.get(0);

        System.out.println(wallet);

        // Load the block chain, if there is one stored locally.
        System.out.println("Reading block store from disk");
        BlockStore blockStore = new BoundedOverheadBlockStore(params, new File(filePrefix
                + ".blockchain"));

        // Connect to the localhost node. One minute timeout since we won't try
        // any other peers
        System.out.println("Connecting ...");
        BlockChain chain = new BlockChain(params, wallet, blockStore);

        final PeerGroup peerGroup = new PeerGroup(blockStore, params, chain);
        // peerGroup.addAddress(new PeerAddress(InetAddress.getLocalHost(),
        // 8333));
        // fall back node
        peerGroup.addAddress(new PeerAddress(InetAddress.getByName("98.143.152.19"), 8333));
        peerGroup.start();

        // We want to know when the balance changes.
        wallet.addEventListener(new WalletEventListener() {
            @Override
            public void onCoinsReceived(Wallet w, Transaction tx, BigInteger prevBalance,
                    BigInteger newBalance) {
                // Running on a peer thread.
                assert !newBalance.equals(BigInteger.ZERO);
                // It's impossible to pick one specific identity that you
                // receive coins from in BitCoin as there
                // could be inputs from many addresses. So instead we just pick
                // the first and assume they were all
                // owned by the same person.
                try {
                    TransactionInput input = tx.getInputs().get(0);
                    Address from = input.getFromAddress();
                    BigInteger value = tx.getValueSentToMe(w);
                    System.out.println("Received " + Utils.bitcoinValueToFriendlyString(value)
                            + " from " + from.toString());
                    // Now send the coins back!
                    Transaction sendTx = w.sendCoins(peerGroup, from, value);
                    assert sendTx != null; // We should never try to send more
                                           // coins than we have!
                    System.out.println("Sent coins back! Transaction hash is "
                            + sendTx.getHashAsString());
                    w.saveToFile(walletFile);
                } catch (ScriptException e) {
                    // If we didn't understand the scriptSig, just crash.
                    e.printStackTrace();
                    throw new RuntimeException(e);
                } catch (IOException e) {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }
            }
        });

        peerGroup.downloadBlockChain();
        System.out.println("Send coins to: " + key.toAddress(params).toString());
        System.out.println("Waiting for coins to arrive. Press Ctrl-C to quit.");
        // The PeerGroup thread keeps us alive until something kills the
        // process.
    }

    public String getWalletFilename() {
        return walletFilename;
    }

    public Wallet getWallet() {
        return wallet;
    }
}
