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

import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

import com.google.bitcoin.core.MultiBitTestUtils.BlockPair;
import com.google.bitcoin.core.WalletTransaction.Pool;
import com.google.bitcoin.store.BlockStore;
import com.google.bitcoin.store.MemoryBlockStore;
import com.google.bitcoin.utils.BriefLogFormatter;
import com.google.common.collect.Lists;

public class WalletTest {
    static final NetworkParameters params = NetworkParameters.unitTests();

    private Address myAddress;
    private Wallet wallet;
   
    private BlockChain chain;
    private BlockStore blockStore;
    private ECKey myKey;

    private Transaction sendMoneyToWallet(Wallet wallet, Transaction tx, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        if (type == null) {
            // Pending/broadcast tx.
            if (wallet.isPendingTransactionRelevant(tx))
                wallet.receivePending(tx, new ArrayList<Transaction>());
        } else {
            BlockPair bp = MultiBitTestUtils.createFakeBlock(params, blockStore, tx);
            wallet.receiveFromBlock(tx, bp.storedBlock, type);
            if (type == AbstractBlockChain.NewBlockType.BEST_CHAIN)
                wallet.notifyNewBestBlock(bp.block);
        }
        return tx;
    }

    private Transaction sendMoneyToWallet(Wallet wallet, BigInteger value, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        return sendMoneyToWallet(wallet, MultiBitTestUtils.createFakeTx(params, value, myAddress), type);
    }

    @Before
    public void setUp() throws Exception {
        myKey = new ECKey();
        myAddress = myKey.toAddress(params);
        wallet = new Wallet(params);
        wallet.addKey(myKey);

        wallet = new Wallet(params);
        wallet.addKey(myKey);
        
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
            BlockPair bp = MultiBitTestUtils.createFakeBlock(params, blockStore, tx);
            wallet.receiveFromBlock(tx, bp.storedBlock, type);
            if (type == AbstractBlockChain.NewBlockType.BEST_CHAIN)
                wallet.notifyNewBestBlock(bp.block);
        }
        return tx;
    }

    private Transaction sendMoneyToWallet(BigInteger value, AbstractBlockChain.NewBlockType type) throws IOException,
            ProtocolException, VerificationException {
        return sendMoneyToWallet(MultiBitTestUtils.createFakeTx(params, value, myAddress), type);
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
        assertEquals(wallet.getChangeAddress(), t2.getOutput(1).getScriptPubKey().getToAddress());

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
        BlockPair bp = MultiBitTestUtils.createFakeBlock(params, blockStore, t2, t3);
        wallet.receiveFromBlock(t2, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        wallet.receiveFromBlock(t3, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        wallet.notifyNewBestBlock(bp.block);
        assertTrue(wallet.isConsistent());
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
        Transaction t1 = MultiBitTestUtils.createFakeTx(params, v1, myAddress);
        Transaction t2 = MultiBitTestUtils.createFakeTx(params, v2, myAddress);
        StoredBlock b1 = MultiBitTestUtils.createFakeBlock(params, blockStore, t1).storedBlock;
        StoredBlock b2 = MultiBitTestUtils.createFakeBlock(params, blockStore, t2).storedBlock;
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
        StoredBlock b3 = MultiBitTestUtils.createFakeBlock(params, blockStore, spend).storedBlock;
        wallet.receiveFromBlock(spend, b3, BlockChain.NewBlockType.BEST_CHAIN);

        // Change is confirmed. We started with 5.50 so we should have 4.50 left.
        BigInteger v4 = toNanoCoins(4, 50);
        assertEquals(v4, wallet.getBalance(Wallet.BalanceType.AVAILABLE));
    }

    // Intuitively you'd expect to be able to create a transaction with identical inputs and outputs and get an
    // identical result to the official client. However the signatures are not deterministic - signing the same data
    // with the same key twice gives two different outputs. So we cannot prove bit-for-bit compatibility in this test
    // suite.

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
        Transaction tx = MultiBitTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
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

    @Test
    public void pending1() throws Exception {
        // Check that if we receive a pending transaction that is then confirmed, we are notified as appropriate.
        final BigInteger nanos = Utils.toNanoCoins(1, 0);
        final Transaction t1 = MultiBitTestUtils.createFakeTx(params, nanos, myAddress);

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
        com.google.bitcoin.core.MultiBitTestUtils.BlockPair fakeBlock = MultiBitTestUtils.createFakeBlock(params, blockStore, t1Copy);
        wallet.receiveFromBlock(t1Copy, fakeBlock.storedBlock, BlockChain.NewBlockType.BEST_CHAIN);
        wallet.notifyNewBestBlock(fakeBlock.block);
        assertFalse(flags[0]);
        assertTrue(flags[1]);
        assertEquals(TransactionConfidence.ConfidenceType.BUILDING, notifiedTx[0].getConfidence().getConfidenceType());
        // Check we don't get notified about an irrelevant transaction.
        flags[0] = false;
        flags[1] = false;
        Transaction irrelevant = MultiBitTestUtils.createFakeTx(params, nanos, new ECKey().toAddress(params));
        if (wallet.isPendingTransactionRelevant(irrelevant))
            wallet.receivePending(irrelevant, null);
        assertFalse(flags[0]);
        assertEquals(2, walletChanged[0]);
    }

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
        Transaction t1 = MultiBitTestUtils.createFakeTx(params, nanos, myAddress);
        StoredBlock b1 = MultiBitTestUtils.createFakeBlock(params, blockStore, t1).storedBlock;
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

    @Test
    public void transactionsList() throws Exception {
        // Check the wallet can give us an ordered list of all received transactions.
        Utils.rollMockClock(0);
        // Receive a coin.
        Transaction tx1 = MultiBitTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
        StoredBlock b1 = MultiBitTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
        wallet.receiveFromBlock(tx1, b1, BlockChain.NewBlockType.BEST_CHAIN);
        // Receive half a coin 10 minutes later.
        Utils.rollMockClock(60 * 10);
        Transaction tx2 = MultiBitTestUtils.createFakeTx(params, Utils.toNanoCoins(0, 5), myAddress);
        StoredBlock b2 = MultiBitTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
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

    @Test
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

    @Test
    public void respectMaxStandardSize() throws Exception {
        // Check that we won't create txns > 100kb. Average tx size is ~220 bytes so this would have to be enormous.
        sendMoneyToWallet(Utils.toNanoCoins(100, 0), AbstractBlockChain.NewBlockType.BEST_CHAIN);
        Transaction tx = new Transaction(params);
        byte[] bits = new byte[20];
        new Random().nextBytes(bits);
        BigInteger v = Utils.toNanoCoins(0, 1);
        // 3100 outputs to a random address.
        for (int i = 0; i < 3100; i++) {
            tx.addOutput(v, new Address(params, bits));
        }
        Wallet.SendRequest req = Wallet.SendRequest.forTx(tx);
        assertFalse(wallet.completeTx(req));
    }

    // There is a test for spending a coinbase transaction as it matures in BlockChainTest#coinbaseTransactionAvailability

    // Support for offline spending is tested in PeerGroupTest
}
