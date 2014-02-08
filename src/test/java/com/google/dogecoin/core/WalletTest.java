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

package com.google.dogecoin.core;

import com.google.dogecoin.core.CoreTestUtils.BlockPair;
import com.google.dogecoin.core.WalletTransaction.Pool;
import com.google.dogecoin.crypto.KeyCrypter;
import com.google.dogecoin.crypto.KeyCrypterException;
import com.google.dogecoin.crypto.KeyCrypterScrypt;
import com.google.dogecoin.store.BlockStore;
import com.google.dogecoin.store.MemoryBlockStore;
import com.google.dogecoin.utils.BriefLogFormatter;
import com.google.dogecoin.utils.Threading;
import com.google.common.collect.Lists;
import com.google.protobuf.ByteString;
import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.ScryptParameters;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.junit.Before;
import org.junit.Test;
import org.spongycastle.crypto.params.KeyParameter;

import java.io.IOException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.security.SecureRandom;
import java.util.*;

import static com.google.dogecoin.core.Utils.toNanoCoins;
import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

/**
 * This is a selection of the bitcoinj wallet tests - mainly included 
 * to exercise the Wallet code every time the multibit tests are run.
 * 
 * The full wallet tests are in the bitcoinj codebase.
 * 
 * @author jim
 *
 */
public class WalletTest {
    static final NetworkParameters params = NetworkParameters.unitTests();

    private Address myAddress;
    private Wallet wallet;
    private Wallet encryptedWallet;
    // A wallet with an initial unencrypted private key and an encrypted private key.
    private Wallet encryptedHeterogeneousWallet;

    private BlockStore blockStore;
    private ECKey myKey;
    private ECKey myEncryptedKey;

    private ECKey myKey2;
    private ECKey myEncryptedKey2;

    private Address myEncryptedAddress;
    private Address myEncryptedAddress2;
    
    private static CharSequence PASSWORD1 = "my helicopter contains eels";
    private static CharSequence WRONG_PASSWORD = "nothing no one nobody nowhere";

    private KeyCrypter keyCrypter;
    private KeyParameter aesKey;
    private KeyParameter wrongAesKey;

    private SecureRandom secureRandom = new SecureRandom();

    private Transaction sendMoneyToWallet(Wallet wallet, Transaction tx, AbstractBlockChain.NewBlockType type) throws IOException,
            VerificationException {
        if (type == null) {
            // Pending/broadcast tx.
            if (wallet.isPendingTransactionRelevant(tx))
                wallet.receivePending(tx, new ArrayList<Transaction>());
        } else {
            BlockPair bp = com.google.dogecoin.core.CoreTestUtils.createFakeBlock(params, blockStore, tx);
            wallet.receiveFromBlock(tx, bp.storedBlock, type, 1);
            if (type == AbstractBlockChain.NewBlockType.BEST_CHAIN)
                wallet.notifyNewBestBlock(bp.storedBlock);
        }
        return tx;
    }

    private Transaction sendMoneyToWallet(Transaction tx, AbstractBlockChain.NewBlockType type) throws IOException,
            VerificationException {
        return sendMoneyToWallet(this.wallet, tx, type);
    }

    private Transaction sendMoneyToWallet(Wallet wallet, BigInteger value, Address toAddress, AbstractBlockChain.NewBlockType type)
            throws IOException, VerificationException {
        return sendMoneyToWallet(wallet, com.google.dogecoin.core.CoreTestUtils.createFakeTx(params, value, toAddress), type);
    }

    private Transaction sendMoneyToWallet(BigInteger value, AbstractBlockChain.NewBlockType type) throws IOException,
            VerificationException {
        return sendMoneyToWallet(this.wallet, com.google.dogecoin.core.CoreTestUtils.createFakeTx(params, value, myAddress), type);
    }

    @Before
    public void setUp() throws Exception {
        myKey = new ECKey();
        myKey2 = new ECKey();
        myAddress = myKey.toAddress(params);
        wallet = new Wallet(params);
        wallet.addKey(myKey);

        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();

        keyCrypter = new KeyCrypterScrypt(scryptParameters);

        wallet = new Wallet(params);
        encryptedWallet = new Wallet(params, keyCrypter);
        encryptedHeterogeneousWallet = new Wallet(params, keyCrypter);

        aesKey = keyCrypter.deriveKey(PASSWORD1);
        wrongAesKey = keyCrypter.deriveKey(WRONG_PASSWORD);

        wallet.addKey(myKey);

        myEncryptedKey = encryptedWallet.addNewEncryptedKey(keyCrypter, aesKey);
        myEncryptedAddress = myEncryptedKey.toAddress(params);
        
        encryptedHeterogeneousWallet.addKey(myKey2);
        myEncryptedKey2 = encryptedHeterogeneousWallet.addNewEncryptedKey(keyCrypter, aesKey);
        myEncryptedAddress2 = myEncryptedKey2.toAddress(params);
        
        blockStore = new MemoryBlockStore(params);
        BriefLogFormatter.init();
    }

    @Test
    public void basicSpending() throws Exception {
        basicSpendingCommon(wallet, myAddress, false);
    }

    @Test
    public void basicSpendingWithEncryptedWallet() throws Exception {
        basicSpendingCommon(encryptedWallet, myEncryptedAddress, true);
    }

    @Test
    public void basicSpendingWithEncryptedHetergeneousWallet() throws Exception {
        basicSpendingCommon(encryptedHeterogeneousWallet, myEncryptedAddress2, true);
    }

    private void basicSpendingCommon(Wallet wallet, Address toAddress, boolean testEncryption) throws Exception {
        // We'll set up a wallet that receives a coin, then sends a coin of
        // lesser value and keeps the change. We
        // will attach a small fee. Because the Bitcoin protocol makes it
        // difficult to determine the fee of an
        // arbitrary transaction in isolation, we'll check that the fee was set
        // by examining the size of the change.

        // Receive some money as a pending transaction.
        receiveAPendingTransaction(wallet, toAddress);

        // Prepare to send.
        Address destination = new ECKey().toAddress(params);
        BigInteger v2 = toNanoCoins(0, 50);
        Wallet.SendRequest req = Wallet.SendRequest.to(destination, v2);
        req.fee = toNanoCoins(0, 1);

        if (testEncryption) {
            // Try to create a send with a fee but no password (this should fail).
            try {
                wallet.completeTx(req);
                fail("No exception was thrown trying to sign an encrypted key with no password supplied.");
            } catch (KeyCrypterException kce) {
                assertEquals("This ECKey is encrypted but no decryption key has been supplied.", kce.getMessage());
            }
            assertEquals("Wrong number of UNSPENT.1", 1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
            assertEquals("Wrong number of ALL.1", 1, wallet.getPoolSize(WalletTransaction.Pool.ALL));

            // Try to create a send with a fee but the wrong password (this should fail).
            req = Wallet.SendRequest.to(destination, v2);
            req.aesKey = wrongAesKey;
            req.fee = toNanoCoins(0, 1);

            try {
                wallet.completeTx(req);
                fail("No exception was thrown trying to sign an encrypted key with the wrong password supplied.");
            } catch (KeyCrypterException kce) {
                assertEquals("Could not decrypt bytes", kce.getMessage());
            }

            assertEquals("Wrong number of UNSPENT.2", 1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
            assertEquals("Wrong number of ALL.2", 1, wallet.getPoolSize(WalletTransaction.Pool.ALL));

            // Create a send with a fee with the correct password (this should succeed).
            req = Wallet.SendRequest.to(destination, v2);
            req.aesKey = aesKey;
            req.fee = toNanoCoins(0, 1);
        }

        // Complete the transaction successfully.
        wallet.completeTx(req);

        Transaction t2 = req.tx;
        assertEquals("Wrong number of UNSPENT.3", 1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Wrong number of ALL.3", 1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(TransactionConfidence.Source.SELF, t2.getConfidence().getSource());
        assertEquals(wallet.getChangeAddress(), t2.getOutput(1).getScriptPubKey().getToAddress(wallet.getNetworkParameters()));

        // Do some basic sanity checks.
        basicSanityChecks(wallet, t2, toAddress, destination);

        // Broadcast the transaction and commit.
        broadcastAndCommit(wallet, t2);

        // Now check that we can spend the unconfirmed change, with a new change
        // address of our own selection.
        // (req.aesKey is null for unencrypted / the correct aesKey for encrypted.)
        spendUnconfirmedChange(wallet, t2, req.aesKey);
    }

    private void receiveAPendingTransaction(Wallet wallet, Address toAddress) throws Exception {
        BigInteger v1 = Utils.toNanoCoins(1, 0);
        Transaction t1 = sendMoneyToWallet(wallet, v1, toAddress, null);
        assertEquals(BigInteger.ZERO, wallet.getBalance());
        assertEquals(v1, wallet.getBalance(Wallet.BalanceType.ESTIMATED));
        assertEquals(1, wallet.getPoolSize(Pool.PENDING));
        assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        sendMoneyToWallet(wallet, t1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals("Incorrect confirmed tx balance", v1, wallet.getBalance());
        assertEquals("Incorrect confirmed tx PENDING pool size", 0, wallet.getPoolSize(WalletTransaction.Pool.PENDING));
        assertEquals("Incorrect confirmed tx UNSPENT pool size", 1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals("Incorrect confirmed tx ALL pool size", 1, wallet.getPoolSize(WalletTransaction.Pool.ALL));
    }

    private void basicSanityChecks(Wallet wallet, Transaction t, Address fromAddress, Address destination) throws VerificationException {
        assertEquals("Wrong number of tx inputs", 1, t.getInputs().size());
        assertEquals(fromAddress, t.getInputs().get(0).getScriptSig().getFromAddress(wallet.getNetworkParameters()));
        assertEquals(t.getConfidence().getConfidenceType(), TransactionConfidence.ConfidenceType.UNKNOWN);
        assertEquals("Wrong number of tx outputs",2, t.getOutputs().size());
        assertEquals(destination, t.getOutputs().get(0).getScriptPubKey().getToAddress(wallet.getNetworkParameters()));
        assertEquals(wallet.getChangeAddress(), t.getOutputs().get(1).getScriptPubKey().getToAddress(wallet.getNetworkParameters()));
        assertEquals(toNanoCoins("0.4899"), t.getOutputs().get(1).getValue());
        // Check the script runs and signatures verify.
        t.getInputs().get(0).verify();
    }

    private void broadcastAndCommit(Wallet wallet, Transaction t) throws Exception {
        final LinkedList<Transaction> txns = Lists.newLinkedList();
        wallet.addEventListener(new AbstractWalletEventListener() {
            @Override
            public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
                txns.add(tx);
            }
        });

        t.getConfidence().markBroadcastBy(new PeerAddress(InetAddress.getByAddress(new byte[]{1,2,3,4})));
        t.getConfidence().markBroadcastBy(new PeerAddress(InetAddress.getByAddress(new byte[]{10,2,3,4})));
        wallet.commitTx(t);
        Threading.waitForUserCode();
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.SPENT));
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(t, txns.getFirst());
        assertEquals(1, txns.size());
    }

    private void spendUnconfirmedChange(Wallet wallet, Transaction t2, KeyParameter aesKey) throws Exception {
        assertEquals(toNanoCoins("0.4899"), wallet.getBalance());
        Wallet.SendRequest req = Wallet.SendRequest.to(new ECKey().toAddress(params), toNanoCoins(0, 48));
        req.aesKey = aesKey;
        Address a = req.changeAddress = new ECKey().toAddress(params);
        wallet.completeTx(req);
        Transaction t3 = req.tx;
        assertEquals(a, t3.getOutput(1).getScriptPubKey().getToAddress(wallet.getNetworkParameters()));
        assertNotNull(t3);
        wallet.commitTx(t3);
        assertTrue(wallet.isConsistent());
        // t2 and t3 gets confirmed in the same block.
        CoreTestUtils.BlockPair bp = CoreTestUtils.createFakeBlock(params, blockStore, t2, t3);
        wallet.receiveFromBlock(t2, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN, 1);
        wallet.receiveFromBlock(t3, bp.storedBlock, AbstractBlockChain.NewBlockType.BEST_CHAIN, 1);
        wallet.notifyNewBestBlock(bp.storedBlock);
        assertTrue(wallet.isConsistent());
    }

    @Test
    public void sideChain() throws Exception {
        // The wallet receives a coin on the main chain, then on a side chain. Balance is equal to both added together
        // as we assume the side chain tx is pending and will be included shortly.
        BigInteger v1 = Utils.toNanoCoins(1, 0);
        sendMoneyToWallet(v1, AbstractBlockChain.NewBlockType.BEST_CHAIN);
        assertEquals(v1, wallet.getBalance());
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.ALL));

        BigInteger v2 = toNanoCoins(0, 50);
        sendMoneyToWallet(v2, AbstractBlockChain.NewBlockType.SIDE_CHAIN);
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.ALL));
        assertEquals(v1, wallet.getBalance());
        assertEquals(v1.add(v2), wallet.getBalance(Wallet.BalanceType.ESTIMATED));
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
        wallet.receiveFromBlock(t1, b1, BlockChain.NewBlockType.BEST_CHAIN, 1);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        wallet.receiveFromBlock(t2, b2, BlockChain.NewBlockType.BEST_CHAIN, 1);
        assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
        assertEquals(expected, wallet.getBalance());

        // Now spend one coin.
        BigInteger v3 = toNanoCoins(1, 0);
        Transaction spend = wallet.createSend(new ECKey().toAddress(params), v3);
        wallet.commitTx(spend);
        assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));

        // Available and estimated balances should not be the same. We don't
        // check the exact available balance here
        // because it depends on the coin selection algorithm.
        assertEquals(toNanoCoins("4.4999"), wallet.getBalance(Wallet.BalanceType.ESTIMATED));
        assertFalse(wallet.getBalance(Wallet.BalanceType.AVAILABLE).equals(wallet.getBalance(Wallet.BalanceType.ESTIMATED)));

        // Now confirm the transaction by including it into a block.
        StoredBlock b3 = CoreTestUtils.createFakeBlock(params, blockStore, spend).storedBlock;
        wallet.receiveFromBlock(spend, b3, BlockChain.NewBlockType.BEST_CHAIN, 1);

        // Change is confirmed. We started with 5.50 so we should have 4.50
        // left.
        assertEquals(toNanoCoins("4.4999"), wallet.getBalance(Wallet.BalanceType.AVAILABLE));
    }

  @Test
   public void largeBalance() throws Exception {
       // Receive 1700 coins then 100 coin.
       BigInteger v1 = toNanoCoins(1700, 0);
       BigInteger v2 = toNanoCoins(100, 0);
       Transaction t1 = CoreTestUtils.createFakeTx(params, v1, myAddress);
       Transaction t2 = CoreTestUtils.createFakeTx(params, v2, myAddress);
       StoredBlock b1 = CoreTestUtils.createFakeBlock(params, blockStore, t1).storedBlock;
       StoredBlock b2 = CoreTestUtils.createFakeBlock(params, blockStore, t2).storedBlock;
       BigInteger expected = toNanoCoins(1800, 0);
       assertEquals(0, wallet.getPoolSize(WalletTransaction.Pool.ALL));
       wallet.receiveFromBlock(t1, b1, BlockChain.NewBlockType.BEST_CHAIN, 1);
       assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
       wallet.receiveFromBlock(t2, b2, BlockChain.NewBlockType.BEST_CHAIN, 1);
       assertEquals(2, wallet.getPoolSize(WalletTransaction.Pool.UNSPENT));
       assertEquals(expected, wallet.getBalance());

       // Now spend 1799 coin.
       BigInteger v3 = toNanoCoins(1799, 0);
       Transaction spend = wallet.createSend(new ECKey().toAddress(params), v3);
       wallet.commitTx(spend);
       assertEquals(1, wallet.getPoolSize(WalletTransaction.Pool.PENDING));

       // Available and estimated balances should not be the same. We don't
       // check the exact available balance here
       // because it depends on the coin selection algorithm.
       assertEquals(toNanoCoins("0.9999"), wallet.getBalance(Wallet.BalanceType.ESTIMATED));
       assertFalse(wallet.getBalance(Wallet.BalanceType.AVAILABLE).equals(wallet.getBalance(Wallet.BalanceType.ESTIMATED)));

       // Now confirm the transaction by including it into a block.
       StoredBlock b3 = CoreTestUtils.createFakeBlock(params, blockStore, spend).storedBlock;
       wallet.receiveFromBlock(spend, b3, BlockChain.NewBlockType.BEST_CHAIN, 1);

       // Change is confirmed. We started with 1800 and spent 1799 so we should have 0.9999 left
        assertEquals(toNanoCoins("0.9999"), wallet.getBalance(Wallet.BalanceType.AVAILABLE));
    }

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
        assertEquals("Reborn transaction amount incorrect", BigInteger.ZERO.subtract(toNanoCoins("0.1001")), send2.getValue(wallet));
    }

    @Test
    public void transactions() throws Exception {
        // This test covers a bug in which Transaction.getValueSentFromMe was
        // calculating incorrectly.
        Transaction tx = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
        // Now add another output (ie, change) that goes to some other address.
        Address someOtherGuy = new ECKey().toAddress(params);
        TransactionOutput output = new TransactionOutput(params, tx, Utils.toNanoCoins(0, 5), someOtherGuy);
        tx.addOutput(output);
        // Note that tx is no longer valid: it spends more than it imports.
        // However checking transactions balance
        // correctly isn't possible in SPV mode because value is a property of
        // outputs not inputs. Without all
        // transactions you can't check they add up.
        wallet.receiveFromBlock(tx, null, BlockChain.NewBlockType.BEST_CHAIN, 1);
        // Now the other guy creates a transaction which spends that change.
        Transaction tx2 = new Transaction(params);
        tx2.addInput(output);
        tx2.addOutput(new TransactionOutput(params, tx2, Utils.toNanoCoins(0, 5), myAddress));
        // tx2 doesn't send any coins from us, even though the output is in the
        // wallet.
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
        assertEquals(Utils.toNanoCoins("0.9999"), wallet.getBalance());
    }

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
        Threading.waitForUserCode();
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
            @Override
            public void onConfidenceChanged(Transaction tx, ChangeReason reason) {
                flags[1] = true;
            }
        });
        assertEquals(TransactionConfidence.ConfidenceType.PENDING, notifiedTx[0].getConfidence().getConfidenceType());
        final Transaction t1Copy = new Transaction(params, t1.bitcoinSerialize());
        com.google.dogecoin.core.CoreTestUtils.BlockPair fakeBlock = CoreTestUtils.createFakeBlock(params, blockStore, t1Copy);
        wallet.receiveFromBlock(t1Copy, fakeBlock.storedBlock, BlockChain.NewBlockType.BEST_CHAIN, 1);
        wallet.notifyNewBestBlock(fakeBlock.storedBlock);
        assertFalse(flags[0]);
        //assertTrue(flags[1]);
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

    @Test
    public void pending2() throws Exception {
        // Check that if we receive a pending tx we did not send, it updates our
        // spent flags correctly.
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
        wallet.receiveFromBlock(t1, b1, BlockChain.NewBlockType.BEST_CHAIN, 1);
        assertEquals(nanos, wallet.getBalance());
        // Create a spend with them, but don't commit it (ie it's from somewhere
        // else but using our keys). This TX
        // will have change as we don't spend our entire balance.
        BigInteger halfNanos = Utils.toNanoCoins(0, 50);
        Transaction t2 = wallet.createSend(new ECKey().toAddress(params), halfNanos);
        // Now receive it as pending.
        if (wallet.isPendingTransactionRelevant(t2))
            wallet.receivePending(t2, null);
        Threading.waitForUserCode();
        // We received an onCoinsSent() callback.
        assertEquals(t2, txn[0]);
        assertEquals(nanos, bigints[0]);
        assertEquals(Utils.toNanoCoins("0.4999"), bigints[1]);
        // Our balance is now 0.50 BTC less a fee.
        assertEquals(Utils.toNanoCoins("0.4999"), wallet.getBalance(Wallet.BalanceType.ESTIMATED));
    }

    @Test
    public void transactionsList() throws Exception {
        // Check the wallet can give us an ordered list of all received
        // transactions.
        Utils.rollMockClock(0);
        // Receive a coin.
        Transaction tx1 = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(1, 0), myAddress);
        StoredBlock b1 = CoreTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
        wallet.receiveFromBlock(tx1, b1, BlockChain.NewBlockType.BEST_CHAIN, 1);
        // Receive half a coin 10 minutes later.
        Utils.rollMockClock(60 * 10);
        Transaction tx2 = CoreTestUtils.createFakeTx(params, Utils.toNanoCoins(0, 5), myAddress);
        StoredBlock b2 = CoreTestUtils.createFakeBlock(params, blockStore, tx1).storedBlock;
        wallet.receiveFromBlock(tx2, b2, BlockChain.NewBlockType.BEST_CHAIN, 1);
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
        assertEquals(Utils.toNanoCoins("0.9999"), wallet.getBalance());
    }

    @Test
    public void encryptionDecryptionBasic() throws Exception {
        encryptionDecryptionBasicCommon(encryptedWallet);
        encryptionDecryptionBasicCommon(encryptedHeterogeneousWallet);
    }
    
    private void encryptionDecryptionBasicCommon(Wallet wallet) {
        // Check the wallet is initially of WalletType ENCRYPTED.
        assertTrue("Wallet is not an encrypted wallet", wallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Correct password should decrypt first encrypted private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with correct password.2", wallet.checkPassword(PASSWORD1));

        // Incorrect password should not decrypt first encrypted private key.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password.3", !wallet.checkPassword(WRONG_PASSWORD));

        // Decrypt wallet.
        assertTrue("The keyCrypter is missing but should not be", keyCrypter != null);
        wallet.decrypt(aesKey);

        // Wallet should now be unencrypted.
        assertTrue("Wallet is not an unencrypted wallet", wallet.getKeyCrypter() == null);

        // Correct password should not decrypt first encrypted private key as wallet is unencrypted.
         assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with correct password", !wallet.checkPassword(PASSWORD1));

        // Incorrect password should not decrypt first encrypted private key as wallet is unencrypted.
        assertTrue("checkPasswordCanDecryptFirstPrivateKey result is wrong with incorrect password", !wallet.checkPassword(WRONG_PASSWORD));

        // Encrypt wallet.
        wallet.encrypt(keyCrypter, aesKey);

        // Wallet should now be of type WalletType.ENCRYPTED_SCRYPT_AES.
        assertTrue("Wallet is not an encrypted wallet", wallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);
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
            assertTrue("Wrong message in EncrypterDecrypterException", ede.getMessage().contains("Could not decrypt bytes"));
        }
    }

    @Test
    public void encryptionDecryptionCheckExceptions() throws Exception {
        // Check the wallet is currently encrypted
        assertTrue("Wallet is not an encrypted wallet", encryptedWallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES);

        // Decrypt wallet.
        assertTrue("The keyCrypter is missing but should not be", keyCrypter != null);
        encryptedWallet.decrypt(aesKey);

        // Try decrypting it again
        try {
            encryptedWallet.decrypt(aesKey);
            fail("Should not be able to decrypt a decrypted wallet");
        } catch (IllegalStateException e) {
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
        } catch (IllegalStateException e) {
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
        byte[] salt = new byte[KeyCrypterScrypt.SALT_LENGTH];
        secureRandom.nextBytes(salt);
        Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder().setSalt(ByteString.copyFrom(salt));
        ScryptParameters scryptParameters = scryptParametersBuilder.build();

        KeyCrypter keyCrypterDifferent = new KeyCrypterScrypt(scryptParameters);

        ECKey ecKeyDifferent = new ECKey();
        ecKeyDifferent = ecKeyDifferent.encrypt(keyCrypterDifferent, aesKey);

        Iterable<ECKey> keys = encryptedWallet.getKeys();
        Iterator<ECKey> iterator = keys.iterator();
        boolean oneKey = iterator.hasNext();
        iterator.next();
        assertTrue("Wrong number of keys in wallet before key addition", oneKey && !iterator.hasNext());

        try {
            encryptedWallet.addKey(ecKeyDifferent);
            fail("AddKey should have thrown an EncrypterDecrypterException but did not.");
        } catch (KeyCrypterException ede) {
            // Expected behaviour.
        }

        keys = encryptedWallet.getKeys();
        iterator = keys.iterator();
        oneKey = iterator.hasNext();

        iterator.next();
        assertTrue("Wrong number of keys in wallet after key addition", oneKey && !iterator.hasNext());
    }

//    @Test
//    public void ageMattersDuringSelection() throws Exception {
//        // Test that we prefer older coins to newer coins when building spends. This reduces required fees and improves
//        // time to confirmation as the transaction will appear less spammy.
//        final int ITERATIONS = 10;
//        Transaction[] txns = new Transaction[ITERATIONS];
//        for (int i = 0; i < ITERATIONS; i++) {
//            txns[i] = sendMoneyToWallet(Utils.toNanoCoins(1, 0), AbstractBlockChain.NewBlockType.BEST_CHAIN);
//        }
//        // Check that we spend transactions in order of reception.
//        for (int i = 0; i < ITERATIONS; i++) {
//            Transaction spend = wallet.createSend(new ECKey().toAddress(params), Utils.toNanoCoins(1, 0));
//            assertEquals(spend.getInputs().size(), 1);
//            assertEquals("Failed on iteration " + i, spend.getInput(0).getOutpoint().getHash(), txns[i].getHash());
//            wallet.commitTx(spend);
//        }
//    }

    @Test
    public void respectMaxStandardSize() throws Exception {
        // Check that we won't create txns > 100kb. Average tx size is ~220 bytes so this would have to be enormous.
        sendMoneyToWallet(Utils.toNanoCoins(100, 0), AbstractBlockChain.NewBlockType.BEST_CHAIN);
        Transaction tx = new Transaction(params);
        byte[] bits = new byte[20];
        new Random().nextBytes(bits);
        // 3100 outputs to a random address.
        BigInteger v = Utils.toNanoCoins(0, 1);
        for (int i = 0; i < 3100; i++) {
            tx.addOutput(v, new Address(params, bits));
        }
        Wallet.SendRequest req = Wallet.SendRequest.forTx(tx);
        assertFalse(wallet.completeTx(req));
    }

    // There is a test for spending a coinbase transaction as it matures in BlockChainTest#coinbaseTransactionAvailability

    // Support for offline spending is tested in PeerGroupTest
}
