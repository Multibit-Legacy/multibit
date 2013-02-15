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

package com.google.bitcoin.core;

import static com.google.bitcoin.core.Utils.toNanoCoins;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.junit.Before;
import org.junit.Test;
import org.spongycastle.crypto.params.KeyParameter;

import com.google.bitcoin.core.CoreTestUtils;
import com.google.bitcoin.core.CoreTestUtils.BlockPair;
import com.google.bitcoin.core.WalletTransaction.Pool;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.bitcoin.crypto.WalletIsAlreadyDecryptedException;
import com.google.bitcoin.crypto.WalletIsAlreadyEncryptedException;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.MemoryBlockStore;
import com.google.bitcoin.utils.BriefLogFormatter;
import com.google.common.collect.Lists;
import com.google.protobuf.ByteString;

public class WalletTest {
    static final NetworkParameters params = NetworkParameters.unitTests();

    private Address myAddress;
    private Wallet wallet;
    private Wallet encryptedWallet;
    
    private BlockChain chain;
    private BlockStore blockStore;
    private ECKey myKey;
    private ECKey myEncryptedKey;
    private Address myEncryptedAddress;

    private static char[] PASSWORD1 = "my helicopter contains eels".toCharArray();
    private static char[] WRONG_PASSWORD = "nothing noone nobody nowhere".toCharArray();
    
    private KeyCrypter keyCrypter;
    private KeyParameter aesKey;
    private KeyParameter wrongAesKey;
    
    private SecureRandom secureRandom = new SecureRandom();
    
    private Transaction sendMoneyToWallet(Wallet wallet, Transaction tx, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        if (type == null) {
            // Pending/broadcast tx.
            if (wallet.isPendingTransactionRelevant(tx))
                wallet.receivePending(tx, new ArrayList<Transaction>());
        } else {
            BlockPair bp = CoreTestUtils.createFakeBlock(params, blockStore, tx);
            wallet.receiveFromBlock(tx, bp.storedBlock, type);
            if (type == AbstractBlockChain.NewBlockType.BEST_CHAIN)
                wallet.notifyNewBestBlock(bp.block);
        }
        return tx;
    }

    private Transaction sendMoneyToWallet(Wallet wallet, BigInteger value, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        return sendMoneyToWallet(wallet, CoreTestUtils.createFakeTx(params, value, myAddress), type);
    }

    @Before
    public void setUp() throws Exception {
        myKey = new ECKey();
        myAddress = myKey.toAddress(params);
        wallet = new Wallet(params);
        wallet.addKey(myKey);

        byte[] salt = new byte[ScryptParametersConstants.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();

        keyCrypter = new KeyCrypterScrypt(scryptParameters);

        wallet = new Wallet(params);
        encryptedWallet = new Wallet(params, keyCrypter);

        aesKey = keyCrypter.deriveKey(PASSWORD1);
        wrongAesKey = keyCrypter.deriveKey(WRONG_PASSWORD);

        wallet.addKey(myKey);
        
        myEncryptedKey = (new ECKey()).encrypt(keyCrypter, aesKey);
        encryptedWallet.addKey(myEncryptedKey);
        myEncryptedAddress = myEncryptedKey.toAddress(params);

        blockStore = new MemoryBlockStore(params);
        chain = new BlockChain(params, wallet, blockStore);
        BriefLogFormatter.init();
    }
    
    private Transaction sendMoneyToWallet(Transaction tx, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        if (type == null) {
            // Pending/broadcast tx.
            if (wallet.isPendingTransactionRelevant(tx))
                wallet.receivePending(tx, new ArrayList<Transaction>());
        } else {
            BlockPair bp = CoreTestUtils.createFakeBlock(params, blockStore, tx);
            wallet.receiveFromBlock(tx, bp.storedBlock, type);
            if (type == AbstractBlockChain.NewBlockType.BEST_CHAIN)
                wallet.notifyNewBestBlock(bp.block);
        }
        return tx;
    }

    private Transaction sendMoneyToWallet(BigInteger value, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        return sendMoneyToWallet(CoreTestUtils.createFakeTx(params, value, myAddress), type);
    }


    @Test
    public void basicSpending() throws Exception {
        // We'll set up a wallet that receives a coin, then sends a coin of lesser value and keeps the change. We
        // will attach a small fee. Because the Bitcoin protocol makes it difficult to determine the fee of an
        // arbitrary transaction in isolation, we'll check that the fee was set by examining the size of the change.

        // Receive some money as a pending transaction.
        BigInteger v1 = Utils.toNanoCoins(1, 0);
        Transaction t1 = sendMoneyToWallet(v1, null);
        assertEquals(BigInteger.ZERO, wallet.getBalance());
        assertEquals(v1, wallet.getBalance(Wallet.BalanceType.ESTIMATED));
        assertEquals(1, wallet.getPoolSize(Pool.PENDING));
        assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        sendMoneyToWallet(t1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(v1, wallet.getBalance());
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));

        // Create a send with a fee.
        Address destination = new ECKey().toAddress(params);
        BigInteger v2 = toNanoCoins(0, 50);
        Wallet.SendRequest req = Wallet.SendRequest.to(destination, v2);
        req.fee = toNanoCoins(0, 1);
        wallet.completeTx(req);
        Transaction t2 = req.tx;
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(TransactionConfidence.Source.SELF, t2.getConfidence().getSource());

        // Do some basic sanity checks.
        assertEquals(1, t2.getInputs().size());
        assertEquals(myAddress, t2.getInputs().get(0).getScriptSig().getFromAddress());
        assertEquals(t2.getConfidence().getConfidenceType(), TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
        assertEquals(2, t2.getOutputs().size());
        assertEquals(destination, t2.getOutputs().get(0).getScriptPubKey().getToAddress());
        assertEquals(wallet.getChangeAddress(), t2.getOutputs().get(1).getScriptPubKey().getToAddress());
        BigInteger v3 = toNanoCoins(0, 49);
        assertEquals(v3, t2.getOutputs().get(1).getValue());
        // Check the script runs and signatures verify.
        t2.getInputs().get(0).verify();

        final LinkedList<Transaction> txns = Lists.newLinkedList();
        wallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                txns.add(tx);
            }
        });
        // We broadcast the TX over the network, and then commit to it.
        t2.getConfidence().markBroadcastBy(new PeerAddress(InetAddress.getByAddress(new byte[]{1,2,3,4})));
        t2.getConfidence().markBroadcastBy(new PeerAddress(InetAddress.getByAddress(new byte[]{10,2,3,4})));
        wallet.commitTx(t2);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.SPENT));
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(t2, txns.getFirst());
        assertEquals(1, txns.size());

        // Now check that we can spend the unconfirmed change.
        assertEquals(v3, wallet.getBalance());
        Transaction t3 = wallet.createSend(new ECKey().toAddress(params), v3);
        assertNotNull(t3);
        wallet.commitTx(t3);
        assertTrue(wallet.isConsistent());
        // t2 and  t3 gets confirmed in the same block.
        BlockPair bp = CoreTestUtils.createFakeBlock(params, blockStore, t2, t3);
        wallet.receiveFromBlock(t2, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        wallet.receiveFromBlock(t3, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        wallet.notifyNewBestBlock(bp.block);
        assertTrue(wallet.isConsistent());
    }
    
    @Test
    public void basicSpendingWithEncryptedWallet() throws Exception {
        // We'll set up an encrypted wallet that receives a coin, then sends a coin of lesser value and keeps the change. We
        // will attach a small fee. Because the Bitcoin protocol makes it difficult to determine the fee of an
        // arbitrary transaction in isolation, we'll check that the fee was set by examining the size of the change.

        // Receive some money.
        BigInteger v1 = Utils.toNanoCoins(1, 0);
        Transaction t1 = CoreTestUtils.createFakeTx(params, v1, myEncryptedAddress);
        com.google.bitcoin.core.CoreTestUtils.BlockPair bp = CoreTestUtils.createFakeBlock(params, blockStore, t1);
        encryptedWallet.receiveFromBlock(t1, bp.storedBlock, BlockChain.NewBlockType.BEST_CHAIN);
        encryptedWallet.notifyNewBestBlock(bp.block);
        
        System.out.println("encryptedWallet = " + encryptedWallet.toString());
        
        assertEquals("Incorrect confirmed tx PENDING pool size", 0, encryptedWallet.getPoolSize(WalletTransaction.Pool.PENDING));
        assertEquals("Incorrect confirmed tx UNSPENT pool size", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Incorrect confirmed tx ALL pool size", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals("Incorrect confirmed tx balance", v1, encryptedWallet.getBalance());

        // Prepare to send.
        Address destination = new ECKey().toAddress(params);
        BigInteger v2 = toNanoCoins(0, 50);
        Wallet.SendRequest req = Wallet.SendRequest.to(destination, v2);
        req.fee = toNanoCoins(0, 1);

        // Try to create a send with a fee but no password (this should fail).
        try {
            encryptedWallet.completeTx(req);
            fail("No exception was thrown trying to sign an encrypted key with no password supplied.");
        } catch (KeyCrypterException kce) {
            assertEquals("This ECKey is encrypted but no decryption key has been supplied.", kce.getMessage());
        }
        Transaction t2 = req.tx;
        assertTrue("A send transaction was created successfully from an encrypted wallet without a password", !req.isCompleted());
        assertEquals("Wrong number of UNSPENT.1", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Wrong number of ALL.1", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.ALL));

        // Try to create a send with a fee but the wrong password (this should fail).
        req = Wallet.SendRequest.to(destination, v2);
        req.aesKey = wrongAesKey;
        req.fee = toNanoCoins(0, 1);

        try {
            encryptedWallet.completeTx(req);
            fail("No exception was thrown trying to sign an encrypted key with the wrong password supplied.");
        } catch (KeyCrypterException kce) {
            assertEquals("Could not decrypt bytes", kce.getMessage());
        }
        t2 = req.tx;
        assertTrue("A send transaction was created successfully from an encrypted wallet with the wrong password", !req.isCompleted());
        assertEquals("Wrong number of UNSPENT.2", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Wrong number of ALL.2",1, encryptedWallet.getPoolSize(WalletTransaction.Pool.ALL));

        // Create a send with a fee with the correct password (this should succeed).
        req = Wallet.SendRequest.to(destination, v2);
        req.aesKey = aesKey;
        req.fee = toNanoCoins(0, 1);

        encryptedWallet.completeTx(req);
        t2 = req.tx;
        assertTrue("A send transaction was not created from an encrypted wallet with the correct password", req.isCompleted());
        assertEquals("Wrong number of UNSPENT.3", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Wrong number of ALL.3", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.ALL));

        // Do some basic sanity checks.
        assertEquals("Wrong number of tx inputs", 1, t2.getInputs().size());
        assertEquals(myEncryptedAddress, t2.getInputs().get(0).getScriptSig().getFromAddress());
        assertEquals(t2.getConfidence().getConfidenceType(), TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
        assertEquals("Wrong number of tx outputs",2, t2.getOutputs().size());
        assertEquals(destination, t2.getOutputs().get(0).getScriptPubKey().getToAddress());
        assertEquals(encryptedWallet.getChangeAddress(), t2.getOutputs().get(1).getScriptPubKey().getToAddress());
        assertEquals(toNanoCoins(0, 49), t2.getOutputs().get(1).getValue());
        // Check the script runs and signatures verify.
        t2.getInputs().get(0).verify();

        final LinkedList<Transaction> txns = Lists.newLinkedList();
        encryptedWallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                txns.add(tx);
            }
        });
        encryptedWallet.commitTx(t2);
        assertEquals("Wrong number of PENDING.4", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.PENDING));
        assertEquals("Wrong number of SPENT.4", 1, encryptedWallet.getPoolSize(WalletTransaction.Pool.SPENT));
        assertEquals("Wrong number of ALL.4", 2, encryptedWallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(t2, txns.getFirst());
        assertEquals("Wrong number of txes.4",1, txns.size());
    }

    @Test
    public void sideChain() throws Exception {
        // The wallet receives a coin on the main chain, then on a side chain. Only main chain counts towards balance.
        BigInteger v1 = Utils.toNanoCoins(1, 0);
        sendMoneyToWallet(v1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(v1, wallet.getBalance());
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));

        BigInteger v2 = toNanoCoins(0, 50);
        sendMoneyToWallet(v2, AbstractBlockChain.NewBlockType.SIDE_CHAIN);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.INACTIVE));
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.ALL));

        assertEquals(v1, wallet.getBalance());
    }

    @Test
    public void balance() throws Exception {
        // Receive 5 coins then half a coin.
        BigInteger v1 = toNanoCoins(5, 0);
        BigInteger v2 = toNanoCoins(0, 50);
        Transaction t1 = CoreTestUtils.createFakeTx(params, v1, myAddress);
        Transaction t2 = CoreTestUtils.createFakeTx(params, v2, myAddress);
        StoredBlock b1 = CoreTestUtils.createFakeBlock(params, blockStore, t1).storedBlock;
        StoredBlock b2 = CoreTestUtils.createFakeBlock(params, blockStore, t2).storedBlock;
        BigInteger expected = toNanoCoins(5, 50);
        assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        wallet.receiveFromBlock(t1, b1, BlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        wallet.receiveFromBlock(t2, b2, BlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(expected, wallet.getBalance());

        // Now spend one coin.
        BigInteger v3 = toNanoCoins(1, 0);
        Transaction spend = wallet.createSend(new ECKey().toAddress(params), v3);
        wallet.commitTx(spend);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));

        // Available and estimated balances should not be the same. We don't check the exact available balance here
        // because it depends on the coin selection algorithm.
        assertEquals(toNanoCoins(4, 50), wallet.getBalance(Wallet.BalanceType.ESTIMATED));
        assertFalse(wallet.getBalance(Wallet.BalanceType.AVAILABLE).equals(
                    wallet.getBalance(Wallet.BalanceType.ESTIMATED)));

        // Now confirm the transaction by including it into a block.
        StoredBlock b3 = CoreTestUtils.createFakeBlock(params, blockStore, spend).storedBlock;
        wallet.receiveFromBlock(spend, b3, BlockChain.NewBlockType.BEST_CHAIN);

        // Change is confirmed. We started with 5.50 so we should have 4.50 left.
        BigInteger v4 = toNanoCoins(4, 50);
        assertEquals(v4, wallet.getBalance(Wallet.BalanceType.AVAILABLE));
    }

    // Intuitively you'd expect to be able to create a transaction with identical inputs and outputs and get an
    // identical result to the official client. However the signatures are not deterministic - signing the same data
    // with the same key twice gives two different outputs. So we cannot prove bit-for-bit compatibility in this test
    // suite.

//    @Test
//    public void blockChainCatchup() throws Exception {
//        // Test that we correctly process transactions arriving from the chain, with callbacks for inbound and outbound.
//        final BigInteger bigints[] = new BigInteger[4];
//        final Transaction txn[] = new Transaction[2];
//        wallet.addEventListener(new AbstractWalletEventListener() {
//            @Override
//            public void onCoinsReceived(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
//                super.onCoinsReceived(wallet, tx, prevBalance, newBalance);
//                bigints[0] = prevBalance;
//                bigints[1] = newBalance;
//                txn[0] = tx;
//            }
//
//            @Override
//            public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
//                super.onCoinsSent(wallet, tx, prevBalance, newBalance);
//                bigints[2] = prevBalance;
//                bigints[3] = newBalance;
//                txn[1] = tx;
//            }
//        });
//        
//        // Receive some money.
//        BigInteger oneCoin = Utils.toNanoCoins(1, 0);
//        Transaction tx1 = createFakeTx(params, oneCoin, myAddress);
//        StoredBlock b1 = createFakeBlock(params, blockStore, tx1).storedBlock;
//        wallet.receiveFromBlock(tx1, b1, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(null, txn[1]);  // onCoinsSent not called.
//        assertEquals(txn[0].getHash(), tx1.getHash());
//        assertEquals(BigInteger.ZERO, bigints[0]);
//        assertEquals(oneCoin, bigints[1]);
//        // Send 0.10 to somebody else.
//        Transaction send1 = wallet.createSend(new ECKey().toAddress(params), toNanoCoins(0, 10), BigInteger.ZERO, myAddress);
//        // Pretend it makes it into the block chain, our wallet state is cleared but we still have the keys, and we
//        // want to get back to our previous state. We can do this by just not confirming the transaction as
//        // createSend is stateless.
//        txn[0] = txn[1] = null;
//        StoredBlock b2 = createFakeBlock(params, blockStore, send1).storedBlock;
//        wallet.receiveFromBlock(send1, b2, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(bitcoinValueToFriendlyString(wallet.getBalance()), "0.90");
//        assertEquals(null, txn[0]);
//        assertEquals(txn[1].getHash(), send1.getHash());
//        assertEquals(bitcoinValueToFriendlyString(bigints[2]), "1.00");
//        assertEquals(bitcoinValueToFriendlyString(bigints[3]), "0.90");
//        // And we do it again after the catchup.
//        Transaction send2 = wallet.createSend(new ECKey().toAddress(params), toNanoCoins(0, 10), BigInteger.ZERO, myAddress);
//        // What we'd really like to do is prove the official client would accept it .... no such luck unfortunately.
//        wallet.commitTx(send2);
//        StoredBlock b3 = createFakeBlock(params, blockStore, send2).storedBlock;
//        wallet.receiveFromBlock(send2, b3, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(bitcoinValueToFriendlyString(wallet.getBalance()), "0.80");
//    }

    @Test
    public void balances() throws Exception {
        BigInteger nanos = Utils.toNanoCoins(1, 0);
        Transaction tx1 = sendMoneyToWallet(nanos, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals("Value sent to me incorrect", nanos, tx1.getValueSentToMe(wallet, true));
        // Send 0.10 to somebody else.
        Transaction send1 = wallet.createSend(new ECKey().toAddress(params), toNanoCoins(0, 10));
        // Reserialize.
        Transaction send2 = new Transaction(params, send1.bitcoinSerialize());
        assertEquals("Value sent from me incorrect", nanos, send2.getValueSentFromMe(wallet));
        assertEquals("Reborn transaction amount incorrect", BigInteger.ZERO.subtract(toNanoCoins(0, 10)), send2.getValue(wallet));
    }

    @Test
    public void transactions() throws Exception {
        // This test covers a bug in which Transaction.getValueSentFromMe was calculating incorrectly.
        Transaction tx = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
        // Now add another output (ie, change) that goes to some other address.
        Address someOtherGuy = new ECKey().toAddress(params);
        TransactionOutput output = new TransactionOutput(params, tx, Utils.toNanoCoins(0, 5), someOtherGuy);
        tx.addOutput(output);
        // Note that tx is no longer valid: it spends more than it imports. However checking transactions balance
        // correctly isn't possible in SPV mode because value is a property of outputs not inputs. Without all
        // transactions you can't check they add up.
        wallet.receiveFromBlock(tx, null, BlockChain.NewBlockType.BEST_CHAIN);
        // Now the other guy creates a transaction which spends that change.
        Transaction tx2 = new Transaction(params);
        tx2.addInput(output);
        tx2.addOutput(new TransactionOutput(params, tx2, Utils.toNanoCoins(0, 5), myAddress));
        // tx2 doesn't send any coins from us, even though the output is in the wallet.
        assertEquals(Utils.toNanoCoins(0, 0), tx2.getValueSentFromMe(wallet));
    }

    @Test
    public void bounce() throws Exception {
        // This test covers bug 64 (False double spends). Check that if we create a spend and it's immediately sent
        // back to us, this isn't considered as a double spend.
        BigInteger coin1 = Utils.toNanoCoins(1, 0);
        sendMoneyToWallet(coin1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        // Send half to some other guy. Sending only half then waiting for a confirm is important to ensure the tx is
        // in the unspent pool, not pending or spent.
        BigInteger coinHalf = Utils.toNanoCoins(0, 50);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        Address someOtherGuy = new ECKey().toAddress(params);
        Transaction outbound1 = wallet.createSend(someOtherGuy, coinHalf);
        wallet.commitTx(outbound1);
        sendMoneyToWallet(outbound1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        // That other guy gives us the coins right back.
        Transaction inbound2 = new Transaction(params);
        inbound2.addOutput(new TransactionOutput(params, inbound2, coinHalf, myAddress));
        inbound2.addInput(outbound1.getOutputs().get(0));
        sendMoneyToWallet(inbound2, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(coin1, wallet.getBalance());
    }

//    @Test
//    public void finneyAttack() throws Exception {
//        // A Finney attack is where a miner includes a transaction spending coins to themselves but does not
//        // broadcast it. When they find a solved block, they hold it back temporarily whilst they buy something with
//        // those same coins. After purchasing, they broadcast the block thus reversing the transaction. It can be
//        // done by any miner for products that can be bought at a chosen time and very quickly (as every second you
//        // withold your block means somebody else might find it first, invalidating your work).
//        //
//        // Test that we handle the attack correctly: a double spend on the chain moves transactions from pending to dead.
//        // This needs to work both for transactions we create, and that we receive from others.
//        final Transaction[] eventDead = new Transaction[1];
//        final Transaction[] eventReplacement = new Transaction[1];
//        wallet.addEventListener(new AbstractWalletEventListener() {
//            @Override
//            public void onTransactionConfidenceChanged(Wallet wallet, Transaction tx) {
//                super.onTransactionConfidenceChanged(wallet, tx);
//                if (tx.getConfidence().getConfidenceType() ==
//                        TransactionConfidence.ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND) {
//                    eventDead[0] = tx;
//                    eventReplacement[0] = tx.getConfidence().getOverridingTransaction();
//                }
//            }
//        });
//
//        // Receive 1 BTC.
//        BigInteger nanos = Utils.toNanoCoins(1, 0);
//        Transaction t1 = createFakeTx(params, nanos, myAddress);
//        wallet.receiveFromBlock(t1, null, BlockChain.NewBlockType.BEST_CHAIN);
//        // Create a send to a merchant.
//        Transaction send1 = wallet.createSend(new ECKey().toAddress(params), toNanoCoins(0, 50), BigInteger.ZERO);
//        // Create a double spend.
//        Transaction send2 = wallet.createSend(new ECKey().toAddress(params), toNanoCoins(0, 50), BigInteger.ZERO);
//        // Broadcast send1.
//        wallet.commitTx(send1);
//        // Receive a block that overrides it.
//        wallet.receiveFromBlock(send2, null, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(send1, eventDead[0]);
//        assertEquals(send2, eventReplacement[0]);
//        assertEquals(TransactionConfidence.ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND,
//                     send1.getConfidence().getConfidenceType());
//        
//        // Receive 10 BTC.
//        nanos = Utils.toNanoCoins(10, 0);
//
//        // Create a double spending tx.
//        Transaction t2 = new Transaction(params);
//        TransactionOutput o1 = new TransactionOutput(params, t2, nanos, myAddress);
//        t2.addOutput(o1);
//        Transaction prevTx = new Transaction(params);
//        Address someBadGuy = new ECKey().toAddress(params);
//        TransactionOutput prevOut = new TransactionOutput(params, prevTx, nanos, someBadGuy);
//        prevTx.addOutput(prevOut);
//        // Connect it.
//        t2.addInput(prevOut);
//        wallet.receivePending(t2);
//        assertEquals(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN, t2.getConfidence().getConfidenceType());
//        // Receive a tx from a block that overrides it.
//        Transaction t3 = new Transaction(params);
//        TransactionOutput o3 = new TransactionOutput(params, t3, nanos, someBadGuy);
//        t3.addOutput(o3);
//        t3.addInput(prevOut);
//        wallet.receiveFromBlock(t3, null, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(TransactionConfidence.ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND, 
//                     t2.getConfidence().getConfidenceType());
//        assertEquals(t3, t2.getConfidence().getOverridingTransaction());
//    }

    @Test
    public void pending1() throws Exception {
        // Check that if we receive a pending transaction that is then confirmed, we are notified as appropriate.
        final BigInteger nanos = Utils.toNanoCoins(1, 0);
        final Transaction t1 = CoreTestUtils.createFakeTx(params, nanos, myAddress);

        // First one is "called" second is "pending".
        final boolean[] flags = new boolean[2];
        final Transaction[] notifiedTx = new Transaction[1];
        final int[] walletChanged = new int[1];
        wallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public void onCoinsReceived(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                // Check we got the expected transaction.
                assertEquals(tx, t1);
                // Check that it's considered to be pending inclusion in the block chain.
                assertEquals(prevBalance, BigInteger.ZERO);
                assertEquals(newBalance, nanos);
                flags[0] = true;
                flags[1] = tx.isPending();
                notifiedTx[0] = tx;
            }

            @Override
            public void onWalletChanged(Wallet wallet) {
                walletChanged[0]++;
            }
        });

        if (wallet.isPendingTransactionRelevant(t1))
            wallet.receivePending(t1, null);
        assertTrue(flags[0]);
        assertTrue(flags[1]);   // is pending
        flags[0] = false;
        // Check we don't get notified if we receive it again.
        assertFalse(wallet.isPendingTransactionRelevant(t1));
        assertFalse(flags[0]);
        // Now check again, that we should NOT be notified when we receive it via a block (we were already notified).
        // However the confidence should be updated.
        // Make a fresh copy of the tx to ensure we're testing realistically.
        flags[0] = flags[1] = false;
        notifiedTx[0].getConfidence().addEventListener(new TransactionConfidence.Listener() {
            public void onConfidenceChanged(Transaction tx) {
                flags[1] = true;
            }
        });
        assertEquals(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN,
                notifiedTx[0].getConfidence().getConfidenceType());
        final Transaction t1Copy = new Transaction(params, t1.bitcoinSerialize());
        com.google.bitcoin.core.CoreTestUtils.BlockPair fakeBlock = CoreTestUtils.createFakeBlock(params, blockStore, t1Copy);
        wallet.receiveFromBlock(t1Copy, fakeBlock.storedBlock, BlockChain.NewBlockType.BEST_CHAIN);
        wallet.notifyNewBestBlock(fakeBlock.block);
        assertFalse(flags[0]);
        assertTrue(flags[1]);
        assertEquals(TransactionConfidence.ConfidenceType.BUILDING, notifiedTx[0].getConfidence().getConfidenceType());
        // Check we don't get notified about an irrelevant transaction.
        flags[0] = false;
        flags[1] = false;
        Transaction irrelevant = CoreTestUtils.createFakeTx(params, nanos, new ECKey().toAddress(params));
        if (wallet.isPendingTransactionRelevant(irrelevant))
            wallet.receivePending(irrelevant, null);
        assertFalse(flags[0]);
        assertEquals(2, walletChanged[0]);
    }
    
//    @Test
//    public void pending1() throws Exception {
//        // Check that if we receive a pending transaction that is then confirmed, we are notified as appropriate.
//        final BigInteger nanos = Utils.toNanoCoins(1, 0);
//        final Transaction t1 = createFakeTx(params, nanos, myAddress);
//
//        // First one is "called" second is "pending".
//        final boolean[] flags = new boolean[2];
//        final Transaction[] notifiedTx = new Transaction[1];
//        wallet.addEventListener(new AbstractWalletEventListener() {
//            @Override
//            public void onCoinsReceived(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
//                // Check we got the expected transaction.
//                assertEquals(tx, t1);
//                // Check that it's considered to be pending inclusion in the block chain.
//                assertEquals(prevBalance, BigInteger.ZERO);
//                assertEquals(newBalance, nanos);
//                flags[0] = true;
//                flags[1] = tx.isPending();
//                notifiedTx[0] = tx;
//            }
//        });
//
//        wallet.receivePending(t1, null);
//        assertTrue(flags[0]);
//        assertTrue(flags[1]);   // is pending
//        flags[0] = false;
//        // Check we don't get notified if we receive it again.
//        wallet.receivePending(t1, null);
//        assertFalse(flags[0]);
//        // Now check again, that we should NOT be notified when we receive it via a block (we were already notified).
//        // However the confidence should be updated.
//        // Make a fresh copy of the tx to ensure we're testing realistically.
//        flags[0] = flags[1] = false;
//        notifiedTx[0].getConfidence().addEventListener(new TransactionConfidence.Listener() {
//            @Override
//            public void onConfidenceChanged(Transaction tx) {
//                flags[1] = true;
//            }
//        });
//        assertEquals(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN,
//                notifiedTx[0].getConfidence().getConfidenceType());
//        final Transaction t1Copy = new Transaction(params, t1.bitcoinSerialize());
//        wallet.receiveFromBlock(t1Copy, createFakeBlock(params, blockStore, t1Copy).storedBlock,
//                BlockChain.NewBlockType.BEST_CHAIN);
//        assertFalse(flags[0]);
//        assertTrue(flags[1]);
//        assertEquals(TransactionConfidence.ConfidenceType.BUILDING, notifiedTx[0].getConfidence().getConfidenceType());
//        // Check we don't get notified about an irrelevant transaction.
//        flags[0] = false;
//        flags[1] = false;
//        Transaction irrelevant = createFakeTx(params, nanos, new ECKey().toAddress(params));
//        wallet.receivePending(irrelevant, null);
//        assertFalse(flags[0]);
//    }

    @Test
    public void pending2() throws Exception {
        // Check that if we receive a pending tx we did not send, it updates our spent flags correctly.
        final Transaction txn[] = new Transaction[1];
        final BigInteger bigints[] = new BigInteger[2];
        wallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                txn[0] = tx;
                bigints[0] = prevBalance;
                bigints[1] = newBalance;
            }
        });
        // Receive some coins.
        BigInteger nanos = Utils.toNanoCoins(1, 0);
        Transaction t1 = CoreTestUtils.createFakeTx(params, nanos, myAddress);
        StoredBlock b1 = CoreTestUtils.createFakeBlock(params, blockStore, t1).storedBlock;
        wallet.receiveFromBlock(t1, b1, BlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(nanos, wallet.getBalance());
        // Create a spend with them, but don't commit it (ie it's from somewhere else but using our keys). This TX
        // will have change as we don't spend our entire balance.
        BigInteger halfNanos = Utils.toNanoCoins(0, 50);
        Transaction t2 = wallet.createSend(new ECKey().toAddress(params), halfNanos);
        // Now receive it as pending.
        wallet.receivePending(t2, null);
        // We received an onCoinsSent() callback.
        assertEquals(t2, txn[0]);
        assertEquals(nanos, bigints[0]);
        assertEquals(halfNanos, bigints[1]);
        // Our balance is now 0.50 BTC
        assertEquals(halfNanos, wallet.getBalance(Wallet.BalanceType.ESTIMATED));
    }

//    @Test
//    public void pending3() throws Exception {
//        // Check that if we receive a pending tx, and it's overridden by a double spend from the main chain, we
//        // are notified that it's dead. This should work even if the pending tx inputs are NOT ours, ie, they don't
//        // connect to anything.
//        BigInteger nanos = Utils.toNanoCoins(1, 0);
//
//        // Create two transactions that share the same input tx.
//        Address badGuy = new ECKey().toAddress(params);
//        Transaction doubleSpentTx = new Transaction(params);
//        TransactionOutput doubleSpentOut = new TransactionOutput(params, doubleSpentTx, nanos, badGuy);
//        doubleSpentTx.addOutput(doubleSpentOut);
//        Transaction t1 = new Transaction(params);
//        TransactionOutput o1 = new TransactionOutput(params, t1, nanos, myAddress);
//        t1.addOutput(o1);
//        t1.addInput(doubleSpentOut);
//        Transaction t2 = new Transaction(params);
//        TransactionOutput o2 = new TransactionOutput(params, t2, nanos, badGuy);
//        t2.addOutput(o2);
//        t2.addInput(doubleSpentOut);
//
//        final Transaction[] called = new Transaction[2];
//        wallet.addEventListener(new AbstractWalletEventListener() {
//            public void onCoinsReceived(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
//                called[0] = tx;
//            }
//
//            @Override
//            public void onTransactionConfidenceChanged(Wallet wallet, Transaction tx) {
//                super.onTransactionConfidenceChanged(wallet, tx);
//                if (tx.getConfidence().getConfidenceType() == 
//                        TransactionConfidence.ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND) {
//                    called[0] = tx;
//                    called[1] = tx.getConfidence().getOverridingTransaction();
//                }
//            }
//        });
//
//        assertEquals(BigInteger.ZERO, wallet.getBalance());
//        wallet.receivePending(t1);
//        assertEquals(t1, called[0]);
//        assertEquals(nanos, wallet.getBalance(Wallet.BalanceType.ESTIMATED));
//        // Now receive a double spend on the main chain.
//        called[0] = called[1] = null;
//        wallet.receiveFromBlock(t2, createFakeBlock(params, blockStore, t2).storedBlock, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(BigInteger.ZERO, wallet.getBalance());
//        assertEquals(t1, called[0]); // dead
//        assertEquals(t2, called[1]); // replacement
//    }

    @Test
    public void transactionsList() throws Exception {
        // Check the wallet can give us an ordered list of all received transactions.
        Utils.rollMockClock(0);
        // Receive a coin.
        Transaction tx1 = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
        StoredBlock b1 = CoreTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
        wallet.receiveFromBlock(tx1, b1, BlockChain.NewBlockType.BEST_CHAIN);
        // Receive half a coin 10 minutes later.
        Utils.rollMockClock(60 * 10);
        Transaction tx2 = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(0, 5), myAddress);
        StoredBlock b2 = CoreTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
        wallet.receiveFromBlock(tx2, b2, BlockChain.NewBlockType.BEST_CHAIN);
        // Check we got them back in order.
        List<Transaction> transactions = wallet.getTransactionsByTime();
        assertEquals(tx2, transactions.get(0));
        assertEquals(tx1,  transactions.get(1));
        assertEquals(2, transactions.size());
        // Check we get only the last transaction if we request a subrage.
        transactions = wallet.getRecentTransactions(1, false);
        assertEquals(1, transactions.size());
        assertEquals(tx2,  transactions.get(0));

        // Create a spend five minutes later.
        Utils.rollMockClock(60 * 5);
        Transaction tx3 = wallet.createSend(new ECKey().toAddress(params), Utils.toNanoCoins(0, 5));
        // Does not appear in list yet.
        assertEquals(2, wallet.getTransactionsByTime().size());
        wallet.commitTx(tx3);
        // Now it does.
        transactions = wallet.getTransactionsByTime();
        assertEquals(3, transactions.size());
        assertEquals(tx3, transactions.get(0));

        // Verify we can handle the case of older wallets in which the timestamp is null (guessed from the
        // block appearances list).
        tx1.setUpdateTime(null);
        tx3.setUpdateTime(null);
        // Check we got them back in order.
        transactions = wallet.getTransactionsByTime();
        assertEquals(tx2,  transactions.get(0));
        assertEquals(3, transactions.size());
    }

    @Test
    public void keyCreationTime() throws Exception {
        wallet = new Wallet(params);
        long now = Utils.rollMockClock(0).getTime() / 1000;  // Fix the mock clock.
        // No keys returns current time.
        assertEquals(now, wallet.getEarliestKeyCreationTime());
        Utils.rollMockClock(60);
        wallet.addKey(new ECKey());
        assertEquals(now + 60, wallet.getEarliestKeyCreationTime());
        Utils.rollMockClock(60);
        wallet.addKey(new ECKey());
        assertEquals(now + 60, wallet.getEarliestKeyCreationTime());
    }
    

    @Test
    public void spendToSameWallet() throws Exception {
        // Test that a spend to the same wallet is dealt with correctly.
        // It should appear in the wallet and confirm.
        // This is a bit of a silly thing to do in the real world as all it does is burn a fee but it is perfectly valid.
        BigInteger coin1 = Utils.toNanoCoins(1, 0);
        BigInteger coinHalf = Utils.toNanoCoins(0, 50);
        // Start by giving us 1 coin.
        sendMoneyToWallet(coin1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        // Send half to ourselves. We should then have a balance available to spend of zero.
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        Transaction outbound1 = wallet.createSend(myAddress, coinHalf);
        wallet.commitTx(outbound1);
        // We should have a zero available balance before the next block.
        assertEquals(BigInteger.ZERO, wallet.getBalance());
        sendMoneyToWallet(outbound1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        // We should have a balance of 1 BTC after the block is received.
        assertEquals(coin1, wallet.getBalance());
    }
    
//    @Test
//    public void spendOutputFromPendingTransaction() throws Exception {
//        // We'll set up a wallet that receives a coin, then sends a coin of lesser value and keeps the change.
//        BigInteger v1 = Utils.toNanoCoins(1, 0);
//        Transaction t1 = createFakeTx(params, v1, myAddress);
//
//        wallet.receiveFromBlock(t1, null, BlockChain.NewBlockType.BEST_CHAIN);
//        assertEquals(v1, wallet.getBalance());
//        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
//        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
//
//        // First create our current transaction
//        ECKey k2 = new ECKey();
//        wallet.addKey(k2);
//        BigInteger v2 = toNanoCoins(0, 50);
//        Transaction t2 = new Transaction(params);
//        TransactionOutput o2 = new TransactionOutput(params, t2, v2, k2.toAddress(params));
//        t2.addOutput(o2);
//        SendRequest request = SendRequest.to(k2.toAddress(params), v2);
//
//        boolean complete = wallet.completeTx(request);
//        assertTrue(complete);
//        
//        // Commit t2, so it is placed in the pending pool
//        wallet.commitTx(t2);
//        assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
//        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));
//        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.ALL));
//        
//        // Now try the spend the output
//        ECKey k3 = new ECKey();
//        BigInteger v3 = toNanoCoins(0, 25);
//        Transaction t3 = new Transaction(params);
//        t3.addOutput(v3, k3.toAddress(params));
//        t3.addInput(o2);
//        t3.signInputs(SigHash.ALL, wallet);
//        
//        // Commit t3, so the coins from the pending t2 are spent
//        wallet.commitTx(t3);
//        assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
//        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.PENDING));
//        assertEquals(3, wallet.getPoolSize(WalletTransaction.Pool.ALL));
//        
//        // Now the output of t2 must not be available for spending
//        assertFalse(o2.isAvailableForSpending());
//    }
    
    @Test
    public void encryptionDecryptionBasic() throws Exception {
        // Check the wallet is initially of WalletType ENCRYPTED.
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Correct password should decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with correct password.2", encryptedWallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));

        // Incorrect password should not decrypt first private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password.3", !encryptedWallet.checkPasswordCanDecryptFirstPrivateKey(WRONG_PASSWORD));

        // Decrypt wallet.
        assertTrue("The keyCrypter is missing but should not be", keyCrypter != null);
        encryptedWallet.decrypt(aesKey);

        // Wallet should now be unencrypted.
        assertTrue("Wallet is not an unencrypted wallet", encryptedWallet.getKeyCrypter() == null);

        // Correct password should not decrypt first private key as wallet is unencrypted.
         assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with correct password", !encryptedWallet.checkPasswordCanDecryptFirstPrivateKey(PASSWORD1));

        // Incorrect password should not decrypt first private key as wallet is unencrypted.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password", !encryptedWallet.checkPasswordCanDecryptFirstPrivateKey(WRONG_PASSWORD));

        // Encrypt wallet.
        encryptedWallet.encrypt(keyCrypter, aesKey);

        // Wallet should now be of type WalletType.ENCRYPTED_SCRYPT_AES.
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
    }

    @Test
    public void encryptionDecryptionBadPassword() throws Exception {
        // Check the wallet is currently encrypted
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Chek that the wrong password does not decrypt the wallet.
        try {
            encryptedWallet.decrypt(wrongAesKey);
            fail("Incorrectly decoded wallet with wrong password");
        } catch (KeyCrypterException ede) {
            assertTrue("Wrong message in EncrypterDecrypterException", ede.getMessage().indexOf("Could not decrypt bytes") > -1);
        }
    }

    @Test
    public void encryptionDecryptionCheckExceptions() throws Exception {
        // Check the wallet is currently encrypted
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Decrypt wallet.
        assertTrue("The keyCrypter is missing but should not be.1", keyCrypter != null);
        encryptedWallet.decrypt(aesKey);

        // Try decrypting it again
        try {
            assertTrue("The keyCrypter is missing but should not be.2", keyCrypter != null);
            encryptedWallet.decrypt(aesKey);
            fail("Should not be able to decrypt a decrypted wallet");
        } catch (WalletIsAlreadyDecryptedException e) {
            assertTrue("Expected behaviour", true);
        }
        assertTrue("Wallet is not an unencrypted wallet.2", encryptedWallet.getKeyCrypter() == null);

        // Encrypt wallet.
        encryptedWallet.encrypt(keyCrypter, aesKey);

        assertTrue("Wallet is not an encrypted wallet.2", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Try encrypting it again
        try {
            encryptedWallet.encrypt(keyCrypter, aesKey);
            fail("Should not be able to encrypt an encrypted wallet");
        } catch (WalletIsAlreadyEncryptedException e) {
            assertTrue("Expected behaviour", true);
        }
        assertTrue("Wallet is not an encrypted wallet.3", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
    }

    @Test
    public void encryptionDecryptionHomogenousKeys() throws Exception {
        // Check the wallet is currently encrypted
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Try added an ECKey that was encrypted with a differenct ScryptParameters (i.e. a non-homogenous key).
        // This is not allowed as the ScryptParameters is stored at the Wallet level.
        byte[] salt = new byte[ScryptParametersConstants.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();

        KeyCrypter keyCrypterDifferent = new KeyCrypterScrypt(scryptParameters);

        ECKey ecKeyDifferent = new ECKey();
        ecKeyDifferent.encrypt(keyCrypterDifferent, aesKey);

        int numberOfKeys = encryptedWallet.getKeychain().size();
        assertTrue("Wrong number of keys in wallet before key addition", numberOfKeys == 1);

        try {
            encryptedWallet.addKey(ecKeyDifferent);
            fail("AddKey should have thrown an EncrypterDecrypterException but did not.");
        } catch (KeyCrypterException ede) {
            // Expected behaviour.
        }

        numberOfKeys = encryptedWallet.getKeychain().size();
        assertTrue("Wrong number of keys in wallet after key addition", numberOfKeys == 1);
    }

    public void ageMattersDuringSelection() throws Exception {
        // Test that we prefer older coins to newer coins when building spends. This reduces required fees and improves
        // time to confirmation as the transaction will appear less spammy.
        final int ITERATIONS = 10;
        Transaction[] txns = new Transaction[ITERATIONS];
        for (int i = 0; i < ITERATIONS; i++) {
            txns[i] = sendMoneyToWallet(Utils.toNanoCoins(1, 0), AbstractBlockChain.NewBlockType.BEST_CHAIN);
        }
        // Check that we spend transactions in order of reception.
        for (int i = 0; i < ITERATIONS; i++) {
            Transaction spend = wallet.createSend(new ECKey().toAddress(params), Utils.toNanoCoins(1, 0));
            assertEquals("Failed on iteration " + i, spend.getInput(0).getOutpoint().getHash(), txns[i].getHash());
            wallet.commitTx(spend);
        }
    }

    // There is a test for spending a coinbase transaction as it matures in BlockChainTest#coinbaseTransactionAvailability

    // Support for offline spending is tested in PeerGroupTest
}
