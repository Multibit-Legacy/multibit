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

import static com.google.bitcoin.core.Utils.bitcoinValueToFriendlyString;
import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;

import org.multibit.IsMultiBitClass;
import org.multibit.MultiBit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spongycastle.crypto.params.KeyParameter;

import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;
import com.google.bitcoin.core.WalletTransaction.Pool;
import com.google.bitcoin.crypto.EncryptedPrivateKey;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.crypto.WalletIsAlreadyDecryptedException;
import com.google.bitcoin.crypto.WalletIsAlreadyEncryptedException;
import com.google.bitcoin.store.WalletProtobufSerializer;
import com.google.bitcoin.utils.EventListenerInvoker;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.ListenableFuture;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;

// To do list:
//
// - Make the keychain member protected and switch it to be a hashmap of some kind so key lookup ops are faster.
// - Refactor how keys are managed to better handle things like deterministic wallets in future.
// - Decompose the class where possible: break logic out into classes that can be customized/replaced by the user.
//     - Coin selection
//     - [Auto]saving to a backing store
//     - Key management
//     - just generally make Wallet smaller and easier to work with
// - Make clearing of transactions able to only rewind the wallet a certain distance instead of all blocks.

/**
 * <p>A Wallet stores keys and a record of transactions that send and receive value from those keys. Using these,
 * it is able to create new transactions that spend the recorded transactions, and this is the fundamental operation
 * of the Bitcoin protocol.</p>
 *
 * <p>To learn more about this class, read <b><a href="http://code.google.com/p/bitcoinj/wiki/WorkingWithTheWallet">
 *     working with the wallet.</a></b></p>
 *
 * <p>To fill up a Wallet with transactions, you need to use it in combination with a {@link BlockChain} and various
 * other objects, see the <a href="http://code.google.com/p/bitcoinj/wiki/GettingStarted">Getting started</a> tutorial
 * on the website to learn more about how to set everything up.</p>
 *
 * <p>Wallets can be serialized using either Java serialization - this is not compatible across versions of bitcoinj,
 * or protocol buffer serialization. You need to save the wallet whenever it changes, there is an auto-save feature
 * that simplifies this for you although you're still responsible for manually triggering a save when your app is about
 * to quit because the auto-save feature waits a moment before actually committing to disk to avoid IO thrashing when
 * the wallet is changing very fast (eg due to a block chain sync). See
 * {@link Wallet#autosaveToFile(java.io.File, long, java.util.concurrent.TimeUnit, com.google.bitcoin.core.Wallet.AutosaveEventListener)}
 * for more information about this.</p>
 */
public class Wallet implements Serializable, IsMultiBitClass {
    private static final Logger log = LoggerFactory.getLogger(Wallet.class);
    private static final long serialVersionUID = 2L;
    
    public static final int MINIMUM_NUMBER_OF_PEERS_A_TRANSACTION_IS_SEEN_BY_FOR_SPEND = 2;

    // Algorithm for movement of transactions between pools. Outbound tx = us spending coins. Inbound tx = us
    // receiving coins. If a tx is both inbound and outbound (spend with change) it is considered outbound for the
    // purposes of the explanation below.
    //
    // 1. Outbound tx is created by us: ->pending
    // 2. Outbound tx that was broadcast is accepted into the main chain:
    //     <-pending  and
    //       If there is a change output  ->unspent
    //       If there is no change output ->spent
    // 3. Outbound tx that was broadcast is accepted into a side chain:
    //     ->inactive  (remains in pending).
    // 4. Inbound tx is accepted into the best chain:
    //     ->unspent/spent
    // 5. Inbound tx is accepted into a side chain:
    //     ->inactive
    //     Whilst it's also 'pending' in some sense, in that miners will probably try and incorporate it into the
    //     best chain, we don't mark it as such here. It'll eventually show up after a re-org.
    // 6. Outbound tx that is pending shares inputs with a tx that appears in the main chain:
    //     <-pending ->dead
    //
    // Re-orgs:
    // 1. Tx is present in old chain and not present in new chain
    //       <-unspent/spent  ->pending
    //       These newly inactive transactions will (if they are relevant to us) eventually come back via receive()
    //       as miners resurrect them and re-include into the new best chain.
    // 2. Tx is not present in old chain and is present in new chain
    //       <-inactive  and  ->unspent/spent
    // 3. Tx is present in new chain and shares inputs with a pending transaction, including those that were resurrected
    //    due to point (1)
    //       <-pending ->dead
    //
    // Balance:
    // 1. Sum up all unspent outputs of the transactions in unspent.
    // 2. Subtract the inputs of transactions in pending.
    // 3. If requested, re-add the outputs of pending transactions that are mine. This is the estimated balance.

    /**
     * Map of txhash->Transactions that have not made it into the best chain yet. They are eligible to move there but
     * are waiting for a miner to create a block on the best chain including them. These transactions inputs count as
     * spent for the purposes of calculating our balance but their outputs are not available for spending yet. This
     * means after a spend, our balance can actually go down temporarily before going up again! We should fix this to
     * allow spending of pending transactions.
     *
     * Pending transactions get announced to peers when they first connect. This means that if we're currently offline,
     * we can still create spends and upload them to the network later.
     */
    final Map<Sha256Hash, Transaction> pending;

    /**
     * Map of txhash->Transactions where the Transaction has unspent outputs. These are transactions we can use
     * to pay other people and so count towards our balance. Transactions only appear in this map if they are part
     * of the best chain. Transactions we have broacast that are not confirmed yet appear in pending even though they
     * may have unspent "change" outputs.<p>
     * <p/>
     * Note: for now we will not allow spends of transactions that did not make it into the block chain. The code
     * that handles this in BitCoin C++ is complicated. Satoshis code will not allow you to spend unconfirmed coins,
     * however, it does seem to support dependency resolution entirely within the context of the memory pool so
     * theoretically you could spend zero-conf coins and all of them would be included together. To simplify we'll
     * make people wait but it would be a good improvement to resolve this in future.
     */
    final Map<Sha256Hash, Transaction> unspent;

    /**
     * Map of txhash->Transactions where the Transactions outputs are all fully spent. They are kept separately so
     * the time to create a spend does not grow infinitely as wallets become more used. Some of these transactions
     * may not have appeared in a block yet if they were created by us to spend coins and that spend is still being
     * worked on by miners.<p>
     * <p/>
     * Transactions only appear in this map if they are part of the best chain.
     */
    final Map<Sha256Hash, Transaction> spent;

    /**
     * An inactive transaction is one that is seen only in a block that is not a part of the best chain. We keep it
     * around in case a re-org promotes a different chain to be the best. In this case some (not necessarily all)
     * inactive transactions will be moved out to unspent and spent, and some might be moved in.<p>
     * <p/>
     * Note that in the case where a transaction appears in both the best chain and a side chain as well, it is not
     * placed in this map. It's an error for a transaction to be in both the inactive pool and unspent/spent.
     */
    final Map<Sha256Hash, Transaction> inactive;

    /**
     * A dead transaction is one that's been overridden by a double spend. Such a transaction is pending except it
     * will never confirm and so should be presented to the user in some unique way - flashing red for example. This
     * should nearly never happen in normal usage. Dead transactions can be "resurrected" by re-orgs just like any
     * other. Dead transactions are not in the pending pool.
     */
    final Map<Sha256Hash, Transaction> dead;

    /**
     * A list of public/private EC keys owned by this user. Access it using addKey[s], hasKey[s] and findPubKeyFromHash.
     */
    public ArrayList<ECKey> keychain;

    private final NetworkParameters params;

    /**
     * The hash of the last block seen on the best chain
     */
    private Sha256Hash lastBlockSeenHash;

    private transient ArrayList<WalletEventListener> eventListeners;

    // A listener that relays confidence changes from the transaction confidence object to the wallet event listener,
    // as a convenience to API users so they don't have to register on every transaction themselves.
    private transient TransactionConfidence.Listener txConfidenceListener;

    // If a TX hash appears in this set then notifyNewBestBlock will ignore it, as its confidence was already set up
    // in receive() via Transaction.setBlockAppearance(). As the BlockChain always calls notifyNewBestBlock even if
    // it sent transactions to the wallet, without this we'd double count.
    private transient HashSet<Sha256Hash> ignoreNextNewBlock;

    /**
     * The type of encryption used in the wallet.
     */
    private EncryptionType encryptionType;

    /**
     * The keyCrypter for the wallet.
     */
    private KeyCrypter keyCrypter;

    /**
     * The wallet version. This is an ordinal used to detect breaking changes
     * in the wallet format.
     */
    WalletVersion version;

    /**
     * A description for the wallet
     */
    String description;

    /**
     * Creates a new, empty wallet with no keys and no transactions. If you want to restore a wallet from disk instead,
     * see loadFromFile.
     */
    public Wallet(NetworkParameters params) {
        this.params = params;
        keychain = new ArrayList<ECKey>();
        unspent = new HashMap<Sha256Hash, Transaction>();
        spent = new HashMap<Sha256Hash, Transaction>();
        inactive = new HashMap<Sha256Hash, Transaction>();
        pending = new HashMap<Sha256Hash, Transaction>();
        dead = new HashMap<Sha256Hash, Transaction>();
        createTransientState();
        
        encryptionType = EncryptionType.UNENCRYPTED;
    }

    /**
     * Create a wallet with a specified keyCrypter.
     * This wallet can then be actually encrypted, with a password, using encrypt().
     */
    public Wallet(NetworkParameters params, KeyCrypter keyCrypter) {
        this(params);
        this.keyCrypter = keyCrypter;
        
        encryptionType = EncryptionType.UNENCRYPTED;
    }

    private void createTransientState() {
        eventListeners = new ArrayList<WalletEventListener>();
        ignoreNextNewBlock = new HashSet<Sha256Hash>();
        txConfidenceListener = new TransactionConfidence.Listener() {
            @Override
            public void onConfidenceChanged(Transaction tx) {
                invokeOnTransactionConfidenceChanged(tx);
                // Many onWalletChanged events will not occur because they are suppressed, eg, because:
                //   - we are inside a re-org
                //   - we are in the middle of processing a block
                //   - the confidence is changing because a new best block was accepted
                // It will run in cases like:
                //   - the tx is pending and another peer announced it
                //   - the tx is pending and was killed by a detected double spend that was not in a block
                // The latter case cannot happen today because we won't hear about it, but in future this may
                // become more common if conflict notices are implemented.
                invokeOnWalletChanged();
            }
        };
    }

    public NetworkParameters getNetworkParameters() {
        return params;
    }

    /**
     * Returns a snapshot of the keychain. This view is not live.
     */
    public synchronized Iterable<ECKey> getKeys() {
        return new ArrayList<ECKey>(keychain);
    }

    /**
     * Uses protobuf serialization to save the wallet to the given file. To learn more about this file format, see
     * {@link WalletProtobufSerializer}.
     * 
     * This method is keep simple as the file saving lifecycle is dealt with in FileHandler.
     */
    public synchronized void saveToFile(File destFile) throws IOException {        
        FileOutputStream stream = null;

        try {
            stream = new FileOutputStream(destFile);
            saveToFileStream(stream);
            // Attempt to force the bits to hit the disk. In reality the OS or hard disk itself may still decide
            // to not write through to physical media for at least a few seconds, but this is the best we can do.
            stream.flush();
            stream.getFD().sync();
            stream.close();
            stream = null;
        } finally {
            if (stream != null) {
                stream.close();
            }
        }
    }

    /**
     * Uses protobuf serialization to save the wallet to the given file stream. To learn more about this file format, see
     * {@link WalletProtobufSerializer}.
     */
    public synchronized void saveToFileStream(OutputStream f) throws IOException {
        new WalletProtobufSerializer().writeWallet(this, f);
    }

    /** Returns the parameters this wallet was created with. */
    public NetworkParameters getParams() {
        return params;
    }

    /**
     * Returns a wallet deserialized from the given file.
     * @throws KeyCrypterException 
     */
    public static Wallet loadFromFile(File f) throws IOException, KeyCrypterException {
        FileInputStream stream = new FileInputStream(f);
        try {
            return loadFromFileStream(stream);
        } finally {
            stream.close();
        }
    }
    
    public boolean isConsistent() {        
        // Seems overzealous so switch off for now.
        return true;
    }

    /**
     * Returns a wallet deserialized from the given input stream.
     * @throws KeyCrypterException 
     */
    public static Wallet loadFromFileStream(InputStream stream) throws IOException, KeyCrypterException {
        // Determine what kind of wallet stream this is: Java Serialization or protobuf format.
        stream = new BufferedInputStream(stream);
        stream.mark(100);
        boolean serialization = stream.read() == 0xac && stream.read() == 0xed;
        stream.reset();

        Wallet wallet;
        
        if (serialization) {
            ObjectInputStream ois = null;
            try {
                ois = new ObjectInputStream(stream);
                wallet = (Wallet) ois.readObject();
            } catch (ClassNotFoundException e) {
                throw new RuntimeException(e);
            } finally {
                if (ois != null) ois.close();
            }
        } else {
            WalletProtobufSerializer walletProtobufSerializer = new WalletProtobufSerializer();
            if (MultiBit.getController() != null && MultiBit.getController().getMultiBitService() != null 
                    && MultiBit.getController().getMultiBitService().getChain() != null) {
                walletProtobufSerializer.setChainHeight(MultiBit.getController().getMultiBitService().getChain().getBestChainHeight());
            }
            wallet = walletProtobufSerializer.readWallet(stream);
        }
        
        if (!wallet.isConsistent()) {
            log.error("Loaded an inconsistent wallet");
        }
        return wallet;
    }

    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
        in.defaultReadObject();
        createTransientState();
    }

    /**
     * Called by the {@link BlockChain} when we receive a new block that sends coins to one of our addresses or
     * spends coins from one of our addresses (note that a single transaction can do both).<p>
     *
     * This is necessary for the internal book-keeping Wallet does. When a transaction is received that sends us
     * coins it is added to a pool so we can use it later to create spends. When a transaction is received that
     * consumes outputs they are marked as spent so they won't be used in future.<p>
     *
     * A transaction that spends our own coins can be received either because a spend we created was accepted by the
     * network and thus made it into a block, or because our keys are being shared between multiple instances and
     * some other node spent the coins instead. We still have to know about that to avoid accidentally trying to
     * double spend.<p>
     *
     * A transaction may be received multiple times if is included into blocks in parallel chains. The blockType
     * parameter describes whether the containing block is on the main/best chain or whether it's on a presently
     * inactive side chain. We must still record these transactions and the blocks they appear in because a future
     * block might change which chain is best causing a reorganize. A re-org can totally change our balance!
     */
    public synchronized void receiveFromBlock(Transaction tx, StoredBlock block,
                                       BlockChain.NewBlockType blockType) throws VerificationException {
        receive(tx, block, blockType, false);
    }

    /**
     * Called when we have found a transaction (via network broadcast or otherwise) that is relevant to this wallet
     * and want to record it. Note that we <b>cannot verify these transactions at all</b>, they may spend fictional
     * coins or be otherwise invalid. They are useful to inform the user about coins they can expect to receive soon,
     * and if you trust the sender of the transaction you can choose to assume they are in fact valid and will not
     * be double spent as an optimization.
     *
     * @param tx
     * @throws VerificationException
     */
    public synchronized void receivePending(Transaction tx) throws VerificationException {
        // Can run in a peer thread.

        // Ignore it if we already know about this transaction. Receiving a pending transaction never moves it
        // between pools.
        EnumSet<Pool> containingPools = getContainingPools(tx);
        if (!containingPools.equals(EnumSet.noneOf(Pool.class))) {
            log.debug("Received tx we already saw in a block or created ourselves: " + tx.getHashAsString());
            return;
        }

        // We only care about transactions that:
        //   - Send us coins
        //   - Spend our coins
        if (!isTransactionRelevant(tx, true)) {
            log.debug("Received tx that isn't relevant to this wallet, discarding.");
            return;
        }

        BigInteger valueSentToMe = tx.getValueSentToMe(this);
        BigInteger valueSentFromMe = tx.getValueSentFromMe(this);
        if (log.isInfoEnabled()) {
            log.info(String.format("Received a pending transaction %s that spends %s BTC from our own wallet," +
                    " and sends us %s BTC", tx.getHashAsString(), Utils.bitcoinValueToFriendlyString(valueSentFromMe),
                    Utils.bitcoinValueToFriendlyString(valueSentToMe)));
        }

        // Mark the tx as having been seen but is not yet in the chain. This will normally have been done already by
        // the Peer before we got to this point, but in some cases (unit tests, other sources of transactions) it may
        // have been missed out.
        TransactionConfidence.ConfidenceType currentConfidence = tx.getConfidence().getConfidenceType();
        if (currentConfidence == TransactionConfidence.ConfidenceType.UNKNOWN) {
            tx.getConfidence().setConfidenceType(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
            // Manually invoke the wallet tx confidence listener here as we didn't yet commit therefore the
            // txConfidenceListener wasn't added.
            invokeOnTransactionConfidenceChanged(tx);
        }

        // If this tx spends any of our unspent outputs, mark them as spent now, then add to the pending pool. This
        // ensures that if some other client that has our keys broadcasts a spend we stay in sync. Also updates the
        // timestamp on the transaction and registers/runs event listeners.
        //
        // Note that after we return from this function, the wallet may have been modified.
        commitTx(tx);
    }

    // Boilerplate that allows event listeners to delete themselves during execution, and auto locks the listener.
    private void invokeOnCoinsReceived(final Transaction tx, final BigInteger balance, final BigInteger newBalance) {
        EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
            @Override public void invoke(WalletEventListener listener) {
                listener.onCoinsReceived(Wallet.this, tx, balance, newBalance);
            }
        });
    }

    private void invokeOnCoinsSent(final Transaction tx, final BigInteger prevBalance, final BigInteger newBalance) {
        EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
            @Override public void invoke(WalletEventListener listener) {
                listener.onCoinsSent(Wallet.this, tx, prevBalance, newBalance);
            }
        });
    }

    /**
     * Returns true if the given transaction sends coins to any of our keys, or has inputs spending any of our outputs,
     * and if includeDoubleSpending is true, also returns true if tx has inputs that are spending outputs which are
     * not ours but which are spent by pending transactions.<p>
     *
     * Note that if the tx has inputs containing one of our keys, but the connected transaction is not in the wallet,
     * it will not be considered relevant.
     */
    public synchronized boolean isTransactionRelevant(Transaction tx, 
            boolean includeDoubleSpending) throws ScriptException {
        return tx.isMine(this) || tx.getValueSentFromMe(this).compareTo(BigInteger.ZERO) > 0
                || tx.getValueSentToMe(this).compareTo(BigInteger.ZERO) > 0
                || (includeDoubleSpending && (findDoubleSpendAgainstPending(tx) != null));
    }

    /**
     * Checks if "tx" is spending any inputs of pending transactions. Not a general check, but it can work even if
     * the double spent inputs are not ours. Returns the pending tx that was double spent or null if none found.
     */
    private Transaction findDoubleSpendAgainstPending(Transaction tx) {
        // Compile a set of outpoints that are spent by tx.
        HashSet<TransactionOutPoint> outpoints = new HashSet<TransactionOutPoint>();
        for (TransactionInput input : tx.getInputs()) {
            outpoints.add(input.getOutpoint());
        }
        // Now for each pending transaction, see if it shares any outpoints with this tx.
        for (Transaction p : pending.values()) {
            for (TransactionInput input : p.getInputs()) {
                if (outpoints.contains(input.getOutpoint())) {
                    // It does, it's a double spend against the pending pool, which makes it relevant.
                    return p;
                }
            }
        }
        return null;
    }

    private synchronized void receive(Transaction tx, StoredBlock block,
                                      BlockChain.NewBlockType blockType,
                                      boolean reorg) throws VerificationException {
        // Runs in a peer thread.
        BigInteger prevBalance = getBalance();

        Sha256Hash txHash = tx.getHash();

        boolean bestChain = blockType == BlockChain.NewBlockType.BEST_CHAIN;
        boolean sideChain = blockType == BlockChain.NewBlockType.SIDE_CHAIN;

        BigInteger valueSentFromMe = tx.getValueSentFromMe(this);
        BigInteger valueSentToMe = tx.getValueSentToMe(this);
        BigInteger valueDifference = valueSentToMe.subtract(valueSentFromMe);

        if (!reorg) {
            log.info("Received tx {} for {} BTC: {}", new Object[]{sideChain ? "on a side chain" : "",
                    bitcoinValueToFriendlyString(valueDifference), tx.getHashAsString()});
        }
        
        // If the transaction is already in our spent or unspent or there is no money in it it is probably
        // due to a block replay so we do not want to do anything with it
        // if it is on a sidechain then let the ELSE below deal with it
        // if it is a double spend it gets processed later
        Transaction doubleSpend = findDoubleSpendAgainstPending(tx);
        boolean alreadyHaveIt = spent.containsKey(tx.getHash()) || unspent.containsKey(tx.getHash());
        boolean noMoneyInItAndNotMine = BigInteger.ZERO.equals(valueSentFromMe) && BigInteger.ZERO.equals(valueSentToMe) && !tx.isMine(this);
        if (bestChain && (doubleSpend == null) && (alreadyHaveIt || noMoneyInItAndNotMine)) {
            log.info("Already have tx " + tx.getHash() + " in spent/ unspent or there is no money in it and it is not mine so ignoring");
            return;
        }

        onWalletChangedSuppressions++;

        // If this transaction is already in the wallet we may need to move it into a different pool. At the very
        // least we need to ensure we're manipulating the canonical object rather than a duplicate.
        Transaction wtx;
        if ((wtx = pending.remove(txHash)) != null) {
            // Make sure "tx" is always the canonical object we want to manipulate, send to event handlers, etc.
            tx = wtx;

            log.info("  <-pending");
            // A transaction we created appeared in a block. Probably this is a spend we broadcast that has been
            // accepted by the network.
            if (bestChain) {
                if (valueSentToMe.equals(BigInteger.ZERO)) {
                    // There were no change transactions so this tx is fully spent.
                    log.info("  ->spent");
                    addWalletTransaction(Pool.SPENT, tx);
                } else {
                    // There was change back to us, or this tx was purely a spend back to ourselves (perhaps for
                    // anonymization purposes).
                    log.info("  ->unspent");
                    addWalletTransaction(Pool.UNSPENT, tx);
                }
            } else if (sideChain) {
                // The transaction was accepted on an inactive side chain, but not yet by the best chain.
                log.info("  ->inactive");
                // It's OK for this to already be in the inactive pool because there can be multiple independent side
                // chains in which it appears:
                //
                //     b1 --> b2
                //        \-> b3
                //        \-> b4 (at this point it's already present in 'inactive'
                boolean alreadyPresent = inactive.put(tx.getHash(), tx) != null;
                if (alreadyPresent)
                    log.info("Saw a transaction be incorporated into multiple independent side chains");
                // Put it back into the pending pool, because 'pending' means 'waiting to be included in best chain'.
                pending.put(tx.getHash(), tx);
            }
        } else {
            // This TX didn't originate with us. It could be sending us coins and also spending our own coins if keys
            // are being shared between different wallets.
            if (sideChain) {
                if (unspent.containsKey(tx.getHash()) || spent.containsKey(tx.getHash())) {
                    // This side chain block contains transactions that already appeared in the best chain. It's normal,
                    // we don't need to consider this transaction inactive, we can just ignore it.
                } else {
                    log.info("  ->inactive");
                    addWalletTransaction(Pool.INACTIVE, tx);
                }
            } else if (bestChain) {
                // Saw a non-pending transaction appear on the best chain, ie, we are replaying the chain or a spend
                // that we never saw broadcast (and did not originate) got included.
                //
                // This can trigger tx confidence listeners to be run in the case of double spends. We may need to
                // delay the execution of the listeners until the bottom to avoid the wallet mutating during updates.
                processTxFromBestChain(tx, doubleSpend);
            }
        }

        log.info("Balance is now: " + bitcoinValueToFriendlyString(getBalance()));

        // WARNING: The code beyond this point can trigger event listeners on transaction confidence objects, which are
        // in turn allowed to re-enter the Wallet. This means we cannot assume anything about the state of the wallet
        // from now on. The balance just received may already be spent.

        if (block != null) {
            // Mark the tx as appearing in this block so we can find it later after a re-org. This also tells the tx
            // confidence object about the block and sets its work done/depth appropriately.
            tx.setBlockAppearance(block, bestChain);
            if (bestChain) {
                // Don't notify this tx of work done in notifyNewBestBlock which will be called immediately after
                // this method has been called by BlockChain for all relevant transactions. Otherwise we'd double
                // count.
                ignoreNextNewBlock.add(txHash);
            }
        }

        // Inform anyone interested that we have received or sent coins but only if:
        //  - This is not due to a re-org.
        //  - The coins appeared on the best chain.
        //  - We did in fact receive some new money.
        //  - We have not already informed the user about the coins when we received the tx broadcast, or for our
        //    own spends. If users want to know when a broadcast tx becomes confirmed, they need to use tx confidence
        //    listeners.
        //
        // TODO: Decide whether to run the event listeners, if a tx confidence listener already modified the wallet.
        boolean wasPending = wtx != null;
        if (!reorg && bestChain && !wasPending) {
            BigInteger newBalance = getBalance();
            int diff = valueDifference.compareTo(BigInteger.ZERO);
            // We pick one callback based on the value difference, though a tx can of course both send and receive
            // coins from the wallet.
            if (diff > 0) {
                invokeOnCoinsReceived(tx, prevBalance, newBalance);
            } else if (diff == 0) {
                // Hack. Invoke onCoinsSent in order to let the client save the wallet. This needs to go away.
                invokeOnCoinsSent(tx, prevBalance, newBalance);
            } else {
                invokeOnCoinsSent(tx, prevBalance, newBalance);
            }
        }

        // Wallet change notification will be sent shortly after the block is finished processing, in notifyNewBestBlock
        onWalletChangedSuppressions--;

        checkState(isConsistent());
    }

    /**
     * <p>Called by the {@link BlockChain} when a new block on the best chain is seen, AFTER relevant wallet
     * transactions are extracted and sent to us UNLESS the new block caused a re-org, in which case this will
     * not be called (the {@link Wallet#reorganize(StoredBlock, java.util.List, java.util.List)} method will
     * call this one in that case).</p>
     *
     * <p>Used to update confidence data in each transaction and last seen block hash. Triggers auto saving.
     * Invokes the onWalletChanged event listener if there were any affected transactions.</p>
     */
    public synchronized void notifyNewBestBlock(Block block) throws VerificationException {
        // Check to see if this block has been seen before.
        Sha256Hash newBlockHash = block.getHash();
        if (newBlockHash.equals(getLastBlockSeenHash()))
            return;
        // Store the new block hash.
        setLastBlockSeenHash(newBlockHash);
        // TODO: Clarify the code below.
        // Notify all the BUILDING transactions of the new block.
        // This is so that they can update their work done and depth.
        onWalletChangedSuppressions++;
        Set<Transaction> transactions = getTransactions(true, false);
        for (Transaction tx : transactions) {
            if (ignoreNextNewBlock.contains(tx.getHash())) {
                // tx was already processed in receive() due to it appearing in this block, so we don't want to
                // notify the tx confidence of work done twice, it'd result in miscounting.
                ignoreNextNewBlock.remove(tx.getHash());
            } else {
                tx.getConfidence().notifyWorkDone(block);
            }
        }
        onWalletChangedSuppressions--;
        invokeOnWalletChanged();
    }

    /**
     * Handle when a transaction becomes newly active on the best chain, either due to receiving a new block or a
     * re-org making inactive transactions active.
     */
    private void processTxFromBestChain(Transaction tx, Transaction doubleSpend) throws VerificationException {
        // This TX may spend our existing outputs even though it was not pending. This can happen in unit
        // tests, if keys are moved between wallets, if we're catching up to the chain given only a set of keys,
        // or if a dead coinbase transaction has moved back onto the main chain.
        boolean isDeadCoinbase = tx.isCoinBase() && dead.containsKey(tx.getHash());
        if (isDeadCoinbase) {
            // There is a dead coinbase tx being received on the best chain. A coinbase tx is made dead when it moves
            // to a side chain but it can be switched back on a reorg and 'resurrected' back to spent or unspent.
            // So take it out of the dead pool.
            log.info("  coinbase tx {} <-dead: confidence {}", tx.getHashAsString(),
                    tx.getConfidence().getConfidenceType().name());
            dead.remove(tx.getHash());
        }

        if (inactive.containsKey(tx.getHash())) {
            // This transaction was seen first on a side chain, but now it's also been seen in the best chain.
            // So we don't need to track it as inactive anymore.
            log.info("  new tx {} <-inactive", tx.getHashAsString());
            inactive.remove(tx.getHash());
        }

        updateForSpends(tx, true);

        if (!tx.getValueSentToMe(this).equals(BigInteger.ZERO)) {
            // It's sending us coins.
            log.info("  new tx {} ->unspent", tx.getHashAsString());
            addWalletTransaction(Pool.UNSPENT, tx);
        } else if (!tx.getValueSentFromMe(this).equals(BigInteger.ZERO)) {
            // It spent some of our coins and did not send us any.
            log.info("  new tx {} ->spent", tx.getHashAsString());
            addWalletTransaction(Pool.SPENT, tx);
        } else {
            // It didn't send us coins nor spend any of our coins. If we're processing it, that must be because it
            // spends outpoints that are also spent by some pending transactions - maybe a double spend of somebody
            // elses coins that were originally sent to us? ie, this might be a Finney attack where we think we
            // received some money and then the sender co-operated with a miner to take back the coins, using a tx
            // that isn't involving our keys at all.
        	// (it can also be an intra-wallet spend - see tx.isMine() call below)
        	
            if (doubleSpend != null) {
                // This is mostly the same as the codepath in updateForSpends, but that one is only triggered when
                // the transaction being double spent is actually in our wallet (ie, maybe we're double spending).
                log.warn("Saw double spend from chain override pending tx {}", doubleSpend.getHashAsString());
                log.warn("  <-pending ->dead");
                pending.remove(doubleSpend.getHash());
                dead.put(doubleSpend.getHash(), doubleSpend);
                // Inform the event listeners of the newly dead tx.
                doubleSpend.getConfidence().setOverridingTransaction(tx);
                invokeOnTransactionConfidenceChanged(doubleSpend);
            } else {
                if (tx.isMine(this)) {
                     // a transaction that does not spend or send us coins but is ours none the less
                     // this can occur when a transaction is sent from outputs in our wallet to
                     // an address in the wallet - it burns a fee but is valid
                     log.info(" new tx -> spent (transfer within wallet - simply burns fee)");
                     boolean alreadyPresent = spent.put(tx.getHash(), tx) != null;
                     assert !alreadyPresent : "TX was received twice (transfer within wallet - simply burns fee";
                     invokeOnTransactionConfidenceChanged(tx);
                } else {
                    throw new IllegalStateException("Received an irrelevant tx that was not a double spend.");
                }
            }
         }
    }

    /**
     * Updates the wallet by checking if this TX spends any of our outputs, and marking them as spent if so. It can
     * be called in two contexts. One is when we receive a transaction on the best chain but it wasn't pending, this
     * most commonly happens when we have a set of keys but the wallet transactions were wiped and we are catching up
     * with the block chain. It can also happen if a block includes a transaction we never saw at broadcast time.
     * If this tx double spends, it takes precedence over our pending transactions and the pending tx goes dead.
     *
     * The other context it can be called is from {@link Wallet#receivePending(Transaction)} ie we saw a tx be
     * broadcast or one was submitted directly that spends our own coins. If this tx double spends it does NOT take
     * precedence because the winner will be resolved by the miners - we assume that our version will win,
     * if we are wrong then when a block appears the tx will go dead.
     */
    private void updateForSpends(Transaction tx, boolean fromChain) throws VerificationException {
        // tx is on the best chain by this point.
        List<TransactionInput> inputs = tx.getInputs();
        for (int i = 0; i < inputs.size(); i++) {
            TransactionInput input = inputs.get(i);
            TransactionInput.ConnectionResult result = input.connect(unspent, TransactionInput.ConnectMode.ABORT_ON_CONFLICT);
            if (result == TransactionInput.ConnectionResult.NO_SUCH_TX) {
                // Not found in the unspent map. Try again with the spent map.
                result = input.connect(spent, TransactionInput.ConnectMode.ABORT_ON_CONFLICT);
                if (result == TransactionInput.ConnectionResult.NO_SUCH_TX) {
                    // Not found in the unspent and spent maps. Try again with the pending map.
                    result = input.connect(pending, TransactionInput.ConnectMode.ABORT_ON_CONFLICT);
                    if (result == TransactionInput.ConnectionResult.NO_SUCH_TX) {
                        // Doesn't spend any of our outputs or is coinbase.
                        continue;
                    }
                }
            }

            if (result == TransactionInput.ConnectionResult.ALREADY_SPENT) {
                // Double spend! Work backwards like so:
                //
                //   A  -> spent by B [pending]
                //     \-> spent by C [chain]
                Transaction doubleSpent = input.getOutpoint().fromTx;   // == A
                checkNotNull(doubleSpent);
                int index = (int) input.getOutpoint().getIndex();
                TransactionOutput output = doubleSpent.getOutputs().get(index);
                TransactionInput spentBy = checkNotNull(output.getSpentBy());
                Transaction connected = checkNotNull(spentBy.getParentTransaction());
                if (fromChain) {
                    // This must have overridden a pending tx, or the block is bad (contains transactions
                    // that illegally double spend: should never occur if we are connected to an honest node).
                    if (pending.containsKey(connected.getHash())) {
                        log.warn("Saw double spend from chain override pending tx {}", connected.getHashAsString());
                        log.warn("  <-pending ->dead");
                        pending.remove(connected.getHash());
                        dead.put(connected.getHash(), connected);
                        // Now forcibly change the connection.
                        input.connect(unspent, TransactionInput.ConnectMode.DISCONNECT_ON_CONFLICT);
                        // Inform the [tx] event listeners of the newly dead tx. This sets confidence type also.
                        connected.getConfidence().setOverridingTransaction(tx);
                    }
                } else {
                    // A pending transaction that tried to double spend our coins - we log and ignore it, because either
                    // 1) The double-spent tx is confirmed and thus this tx has no effect .... or
                    // 2) Both txns are pending, neither has priority. Miners will decide in a few minutes which won.
                    log.warn("Saw double spend from another pending transaction, ignoring tx {}",
                             tx.getHashAsString());
                    log.warn("  offending input is input {}", i);
                    return;
                }
            } else if (result == TransactionInput.ConnectionResult.SUCCESS) {
                // Otherwise we saw a transaction spend our coins, but we didn't try and spend them ourselves yet.
                // The outputs are already marked as spent by the connect call above, so check if there are any more for
                // us to use. Move if not.
                Transaction connected = checkNotNull(input.getOutpoint().fromTx);
                maybeMoveTxToSpent(connected, "prevtx");
            }
        }
    }

    /**
     * If the transactions outputs are all marked as spent, and it's in the unspent map, move it.
     */
    private void maybeMoveTxToSpent(Transaction tx, String context) {
        if (tx.isEveryOwnedOutputSpent(this)) {
            // There's nothing left I can spend in this transaction.
            if (unspent.remove(tx.getHash()) != null) {
                if (log.isInfoEnabled()) {
                    log.info("  {} {} <-unspent", tx.getHashAsString(), context);
                    log.info("  {} {} ->spent", tx.getHashAsString(), context);
                }
                spent.put(tx.getHash(), tx);
            }
        }
    }

    /**
     * Adds an event listener object. Methods on this object are called when something interesting happens,
     * like receiving money.<p>
     * <p/>
     * Threading: Event listener methods are dispatched on library provided threads and the both the wallet and the
     * listener objects are locked during dispatch, so your listeners do not have to be thread safe. However they
     * should not block as the Peer will be unresponsive to network traffic whilst your listener is running.
     */
    public synchronized void addEventListener(WalletEventListener listener) {
        eventListeners.add(listener);
    }

    /**
     * Removes the given event listener object. Returns true if the listener was removed,
     * false if that listener was never added.
     */
    public synchronized boolean removeEventListener(WalletEventListener listener) {
        return eventListeners.remove(listener);
    }

    /**
     * <p>Updates the wallet with the given transaction: puts it into the pending pool, sets the spent flags and runs
     * the onCoinsSent/onCoinsReceived event listener. Used in two situations:</p>
     *
     * <ol>
     *     <li>When we have just successfully transmitted the tx we created to the network.</li>
     *     <li>When we receive a pending transaction that didn't appear in the chain yet, and we did not create it.</li>
     * </ol>
     */
    public synchronized void commitTx(Transaction tx) throws VerificationException {
        checkArgument(!pending.containsKey(tx.getHash()), "commitTx called on the same transaction twice");
        log.info("commitTx of {}", tx.getHashAsString());
        BigInteger balance = getBalance();
        tx.setUpdateTime(Utils.now());
        // Mark the outputs we're spending as spent so we won't try and use them in future creations. This will also
        // move any transactions that are now fully spent to the spent map so we can skip them when creating future
        // spends.
        updateForSpends(tx, false);
        // Add to the pending pool. It'll be moved out once we receive this transaction on the best chain.
        // This also registers txConfidenceListener so wallet listeners get informed.
        log.info("->pending: {}", tx.getHashAsString());
        addWalletTransaction(Pool.PENDING, tx);

        // Event listeners may re-enter so we cannot make assumptions about wallet state after this loop completes.
        try {
            BigInteger valueSentFromMe = tx.getValueSentFromMe(this);
            BigInteger valueSentToMe = tx.getValueSentToMe(this);
            BigInteger newBalance = balance.add(valueSentToMe).subtract(valueSentFromMe);
            if (valueSentToMe.compareTo(BigInteger.ZERO) > 0)
                invokeOnCoinsReceived(tx, balance, newBalance);
            if (valueSentFromMe.compareTo(BigInteger.ZERO) > 0)
                invokeOnCoinsSent(tx, balance, newBalance);

            invokeOnWalletChanged();
        } catch (ScriptException e) {
            // Cannot happen as we just created this transaction ourselves.
            throw new RuntimeException(e);
        }

        checkState(isConsistent());
    }

    /**
     * Returns a set of all transactions in the wallet.
     * @param includeDead     If true, transactions that were overridden by a double spend are included.
     * @param includeInactive If true, transactions that are on side chains (are unspendable) are included.
     */
    public synchronized Set<Transaction> getTransactions(boolean includeDead, boolean includeInactive) {
        Set<Transaction> all = new HashSet<Transaction>();
        all.addAll(unspent.values());
        all.addAll(spent.values());
        all.addAll(pending.values());
        if (includeDead)
            all.addAll(dead.values());
        if (includeInactive)
            all.addAll(inactive.values());
        return all;
    }

    /**
     * Returns a set of all WalletTransactions in the wallet.
     */
    public synchronized Iterable<WalletTransaction> getWalletTransactions() {
        HashSet<Transaction> pendingInactive = new HashSet<Transaction>();
        pendingInactive.addAll(pending.values());
        pendingInactive.retainAll(inactive.values());
        HashSet<Transaction> onlyPending = new HashSet<Transaction>();
        HashSet<Transaction> onlyInactive = new HashSet<Transaction>();
        onlyPending.addAll(pending.values());
        onlyPending.removeAll(pendingInactive);
        onlyInactive.addAll(inactive.values());
        onlyInactive.removeAll(pendingInactive);
        
        Set<WalletTransaction> all = new HashSet<WalletTransaction>();

        addWalletTransactionsToSet(all, Pool.UNSPENT, unspent.values());
        addWalletTransactionsToSet(all, Pool.SPENT, spent.values());
        addWalletTransactionsToSet(all, Pool.DEAD, dead.values());
        addWalletTransactionsToSet(all, Pool.PENDING, onlyPending);
        addWalletTransactionsToSet(all, Pool.INACTIVE, onlyInactive);
        addWalletTransactionsToSet(all, Pool.PENDING_INACTIVE, pendingInactive);
        return all;
    }

    private static synchronized void addWalletTransactionsToSet(Set<WalletTransaction> txs,
            Pool poolType, Collection<Transaction> pool) {
        for (Transaction tx : pool) {
            txs.add(new WalletTransaction(poolType, tx));
        }
    }

    /**
     * Adds a transaction that has been associated with a particular wallet pool. This is intended for usage by
     * deserialization code, such as the {@link WalletProtobufSerializer} class. It isn't normally useful for
     * applications. It does not trigger auto saving.
     */
    public void addWalletTransaction(WalletTransaction wtx) {
        addWalletTransaction(wtx.getPool(), wtx.getTransaction());
    }

    /**
     * Adds the given transaction to the given pools and registers a confidence change listener on it.
     */
    private synchronized void addWalletTransaction(Pool pool, Transaction tx) {
        switch (pool) {
        case UNSPENT:
            unspent.put(tx.getHash(), tx);
            break;
        case SPENT:
            spent.put(tx.getHash(), tx);
            break;
        case PENDING:
            pending.put(tx.getHash(), tx);
            break;
        case DEAD:
            dead.put(tx.getHash(), tx);
            break;
        case INACTIVE:
            inactive.put(tx.getHash(), tx);
            break;
        case PENDING_INACTIVE:
            pending.put(tx.getHash(), tx);
            inactive.put(tx.getHash(), tx);
            break;
        default:
            throw new RuntimeException("Unknown wallet transaction type " + pool);
        }
        // This is safe even if the listener has been added before, as TransactionConfidence ignores duplicate
        // registration requests. That makes the code in the wallet simpler.
        tx.getConfidence().addEventListener(txConfidenceListener);
    }

    /**
     * Returns all non-dead, active transactions ordered by recency.
     */
    public List<Transaction> getTransactionsByTime() {
        return getRecentTransactions(0, false);
    }

    /**
     * Returns an list of N transactions, ordered by increasing age. Transactions on side chains are not included.
     * Dead transactions (overridden by double spends) are optionally included. <p>
     * <p/>
     * Note: the current implementation is O(num transactions in wallet). Regardless of how many transactions are
     * requested, the cost is always the same. In future, requesting smaller numbers of transactions may be faster
     * depending on how the wallet is implemented (eg if backed by a database).
     */
    public synchronized List<Transaction> getRecentTransactions(int numTransactions, boolean includeDead) {
        checkArgument(numTransactions >= 0);
        // Firstly, put all transactions into an array.
        int size = getPoolSize(WalletTransaction.Pool.UNSPENT) +
                getPoolSize(WalletTransaction.Pool.SPENT) +
                getPoolSize(WalletTransaction.Pool.PENDING);
        if (numTransactions > size || numTransactions == 0) {
            numTransactions = size;
        }
        ArrayList<Transaction> all = new ArrayList<Transaction>(getTransactions(includeDead, false));
        // Order by date.
        Collections.sort(all, Collections.reverseOrder(new Comparator<Transaction>() {
            @Override
            public int compare(Transaction t1, Transaction t2) {
                return t1.getUpdateTime().compareTo(t2.getUpdateTime());
            }
        }));
        if (numTransactions == all.size()) {
            return all;
        } else {
            all.subList(numTransactions, all.size()).clear();
            return all;
        }
    }

    /**
     * Returns a transaction object given its hash, if it exists in this wallet, or null otherwise.
     */
    public synchronized Transaction getTransaction(Sha256Hash hash) {
        Transaction tx;
        if ((tx = pending.get(hash)) != null)
            return tx;
        else if ((tx = unspent.get(hash)) != null)
            return tx;
        else if ((tx = spent.get(hash)) != null)
            return tx;
        else if ((tx = inactive.get(hash)) != null)
            return tx;
        else if ((tx = dead.get(hash)) != null)
            return tx;
        return null;
    }

    /**
     * Deletes transactions which appeared above the given block height from the wallet, but does not touch the keys.
     * This is useful if you have some keys and wish to replay the block chain into the wallet in order to pick them up.
     */
    public synchronized void clearTransactions(int fromHeight) {
        if (fromHeight == 0) {
            unspent.clear();
            spent.clear();
            pending.clear();
            inactive.clear();
            dead.clear();
        } else {
            throw new UnsupportedOperationException();
        }
    }

    synchronized EnumSet<Pool> getContainingPools(Transaction tx) {
        EnumSet<Pool> result = EnumSet.noneOf(Pool.class);
        Sha256Hash txHash = tx.getHash();
        if (unspent.containsKey(txHash)) {
            result.add(Pool.UNSPENT);
        }
        if (spent.containsKey(txHash)) {
            result.add(Pool.SPENT);
        }
        if (pending.containsKey(txHash)) {
            result.add(Pool.PENDING);
        }
        if (inactive.containsKey(txHash)) {
            result.add(Pool.INACTIVE);
        }
        if (dead.containsKey(txHash)) {
            result.add(Pool.DEAD);
        }
        return result;
    }

    synchronized int getPoolSize(WalletTransaction.Pool pool) {
        switch (pool) {
            case UNSPENT:
                return unspent.size();
            case SPENT:
                return spent.size();
            case PENDING:
                return pending.size();
            case INACTIVE:
                return inactive.size();
            case DEAD:
                return dead.size();
            case ALL:
                return unspent.size() + spent.size() + pending.size() + inactive.size() + dead.size();
        }
        throw new RuntimeException("Unreachable");
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //  SEND APIS
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /** A SendResult is returned to you as part of sending coins to a recipient. */
    public static class SendResult {
        /** The Bitcoin transaction message that moves the money. */
        public Transaction tx;
        /** A future that will complete once the tx message has been successfully broadcast to the network. */
        public ListenableFuture<Transaction> broadcastComplete;
    }

    /**
     * A SendRequest gives the wallet information about precisely how to send money to a recipient or set of recipients.
     * Static methods are provided to help you create SendRequests and there are a few helper methods on the wallet that
     * just simplify the most common use cases. You may wish to customize a SendRequest if you want to attach a fee or
     * modify the change address.
     */
    public static class SendRequest {
        /**
         * A transaction, probably incomplete, that describes the outline of what you want to do. This typically will
         * mean it has some outputs to the intended destinations, but no inputs or change address (and therefore no
         * fees) - the wallet will calculate all that for you and update tx later.
         */
        public Transaction tx;

        /**
         * "Change" means the difference between the value gathered by a transactions inputs (the size of which you
         * don't really control as it depends on who sent you money), and the value being sent somewhere else. The
         * change address should be selected from this wallet, normally. <b>If null this will be chosen for you.</b>
         */
        public Address changeAddress;

        /**
         * A transaction can have a fee attached, which is defined as the difference between the input values
         * and output values. Any value taken in that is not provided to an output can be claimed by a miner. This
         * is how mining is incentivized in later years of the Bitcoin system when inflation drops. It also provides
         * a way for people to prioritize their transactions over others and is used as a way to make denial of service
         * attacks expensive. Some transactions require a fee due to their structure - currently bitcoinj does not
         * correctly calculate this! As of late 2012 most transactions require no fee.
         */
        public BigInteger fee = BigInteger.ZERO;

        // Tracks if this has been passed to wallet.completeTx already: just a safety check.
        private boolean completed;

        private SendRequest() {}

        public static SendRequest to(Address destination, BigInteger value) {
            SendRequest req = new Wallet.SendRequest();
            req.tx = new Transaction(destination.getParameters());
            req.tx.addOutput(value, destination);
            return req;
        }

        public static SendRequest to(NetworkParameters params, ECKey destination, BigInteger value) {
            SendRequest req = new SendRequest();
            req.tx = new Transaction(params);
            req.tx.addOutput(value, destination);
            return req;
        }

        /** Simply wraps a pre-built incomplete transaction provided by you. */
        public static SendRequest forTx(Transaction tx) {
            SendRequest req = new SendRequest();
            req.tx = tx;
            return req;
        }
    }

    /**
     * Statelessly creates a transaction that sends the given number of nanocoins to address. The change is sent to
     * {@link Wallet#getChangeAddress()}, so you must have added at least one key.<p>
     * <p/>
     * This method is stateless in the sense that calling it twice with the same inputs will result in two
     * Transaction objects which are equal. The wallet is not updated to track its pending status or to mark the
     * coins as spent until commitTx is called on the result.
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized Transaction createSend(Address address, BigInteger nanocoins, final BigInteger fee, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        return createSend(address, nanocoins, fee, getChangeAddress(), aesKey);
    }

    /**
     * Sends coins to the given address but does not broadcast the resulting pending transaction. It is still stored
     * in the wallet, so when the wallet is added to a {@link PeerGroup} or {@link Peer} the transaction will be
     * announced to the network.
     *
     * @param to Address to send the coins to.
     * @param nanocoins How many coins to send.
     * @return the Transaction that was created, or null if there are insufficient coins in thew allet.
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized Transaction sendCoinsOffline(Address to, BigInteger nanocoins, BigInteger fee, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        Transaction tx = createSend(to, nanocoins, fee, aesKey);
        if (tx == null)   // Not enough money! :-(
            return null;
        try {
            commitTx(tx);
        } catch (VerificationException e) {
            throw new RuntimeException(e);  // Cannot happen unless there's a bug, as we just created this ourselves.
        }
        return tx;
    }

    /**
     * Sends coins to the given address, via the given {@link PeerGroup}. Change is returned to {@link Wallet#getChangeAddress()}.
     * The transaction will be announced to any connected nodes asynchronously. If you would like to know when
     * the transaction was successfully sent to at least one node, use 
     * {@link Wallet#sendCoinsOffline(Address, java.math.BigInteger)} and then {@link PeerGroup#broadcastTransaction(Transaction)}
     * on the result to obtain a {@link java.util.concurrent.Future<Transaction>}.
     *
     * @param peerGroup a PeerGroup to use for broadcast.
     * @param to        Which address to send coins to.
     * @param nanocoins How many nanocoins to send. You can use Utils.toNanoCoins() to calculate this.
     * @param fee       The fee to include     
     * @return the Transaction
     * @throws IOException if there was a problem broadcasting the transaction
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized Transaction sendCoinsAsync(PeerGroup peerGroup, Address to, BigInteger nanocoins, BigInteger fee, KeyParameter aesKey)
            throws IOException, IllegalStateException, KeyCrypterException {
        Transaction tx = sendCoinsOffline(to, nanocoins, fee, aesKey);
        if (tx == null)
            return null;  // Not enough money.
        // Just throw away the Future here. If the user wants it, they can call sendCoinsOffline/broadcastTransaction
        // themselves.
        peerGroup.broadcastTransaction(tx);
        return tx;
    }

    /**
     * Sends coins to the given address, via the given {@link PeerGroup}. Change is returned to {@link Wallet#getChangeAddress()}.
     * The method will block until the transaction has been announced to at least one node.
     *
     * @param peerGroup a PeerGroup to use for broadcast or null.
     * @param to        Which address to send coins to.
     * @param nanocoins How many nanocoins to send. You can use Utils.toNanoCoins() to calculate this.
     * @param fee       The fee to include     
     * @return The {@link Transaction} that was created or null if there was insufficient balance to send the coins.
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized Transaction sendCoins(PeerGroup peerGroup, Address to, BigInteger nanocoins, final BigInteger fee, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        Transaction tx = sendCoinsOffline(to, nanocoins, fee, aesKey);
        if (tx == null)
            return null;  // Not enough money.
        try {
            return peerGroup.broadcastTransaction(tx).get();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Sends coins to the given address, via the given {@link Peer}. Change is returned to {@link Wallet#getChangeAddress()}.
     * If an exception is thrown by {@link Peer#sendMessage(Message)} the transaction is still committed, so the
     * pending transaction must be broadcast <b>by you</b> at some other time.
     *
     * @param to        Which address to send coins to.
     * @param nanocoins How many nanocoins to send. You can use Utils.toNanoCoins() to calculate this.
     * @param fee       The fee to include     
     * @return The {@link Transaction} that was created or null if there was insufficient balance to send the coins.
     * @throws IOException if there was a problem broadcasting the transaction
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized Transaction sendCoins(Peer peer, Address to, BigInteger nanocoins, BigInteger fee, KeyParameter aesKey) throws IOException, IllegalStateException, KeyCrypterException {
        // TODO: This API is fairly questionable and the function isn't tested. If anything goes wrong during sending
        // on the peer you don't get access to the created Transaction object and must fish it out of the wallet then
        // do your own retry later.

        Transaction tx = createSend(to, nanocoins, fee, aesKey);
        if (tx == null)   // Not enough money! :-(
            return null;
        try {
            commitTx(tx);
        } catch (VerificationException e) {
            throw new RuntimeException(e);  // Cannot happen unless there's a bug, as we just created this ourselves.
        }
        peer.sendMessage(tx);
        return tx;
    }

    /**
     * Creates a transaction that sends $coins.$cents BTC to the given address.<p>
     * <p/>
     * IMPORTANT: This method does NOT update the wallet. If you call createSend again you may get two transactions
     * that spend the same coins. You have to call commitTx on the created transaction to prevent this,
     * but that should only occur once the transaction has been accepted by the network. This implies you cannot have
     * more than one outstanding sending tx at once.
     *
     * @param address       The BitCoin address to send the money to.
     * @param nanocoins     How much currency to send, in nanocoins.
     * @param fee           The fee to include
     * @param changeAddress Which address to send the change to, in case we can't make exactly the right value from
     *                      our coins. This should be an address we own (is in the keychain).
     * @return a new {@link Transaction} or null if we cannot afford this send.
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    synchronized Transaction createSend(Address address, BigInteger nanocoins, final BigInteger fee, Address changeAddress, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        log.info("Creating send tx to " + address.toString() + " for " +
                bitcoinValueToFriendlyString(nanocoins));

        Transaction sendTx = new Transaction(params);
        sendTx.addOutput(nanocoins, address);

        if (completeTx(sendTx, changeAddress, fee, aesKey)) {
            return sendTx;
        } else {
            return null;
        }
    }

    /**
     * Takes a transaction with arbitrary outputs, gathers the necessary inputs for spending, and signs it
     * @param sendTx           The transaction to complete
     * @param changeAddress    Which address to send the change to, in case we can't make exactly the right value from
     *                         our coins. This should be an address we own (is in the keychain).
     * @param fee              The fee to include     
     * @return False if we cannot afford this send, true otherwise
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized boolean completeTx(Transaction sendTx, Address changeAddress, BigInteger fee, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        // Calculate the transaction total
        BigInteger nanocoins = BigInteger.ZERO;
        for(TransactionOutput output : sendTx.getOutputs()) {
            nanocoins = nanocoins.add(output.getValue());
        }
        final BigInteger total = nanocoins.add(fee);

        log.info("Completing send tx with {} outputs totalling {}", sendTx.getOutputs().size(), bitcoinValueToFriendlyString(nanocoins));

        // To send money to somebody else, we need to do gather up transactions with unspent outputs until we have
        // sufficient value. Many coin selection algorithms are possible, we use a simple but suboptimal one.
        // TODO: Sort coins so we use the smallest first, to combat wallet fragmentation and reduce fees.
        BigInteger valueGathered = BigInteger.ZERO;
        List<TransactionOutput> gathered = new LinkedList<TransactionOutput>();
        for (Transaction tx : unspent.values()) {
            // Do not try and spend coinbases that were mined too recently, the protocol forbids it.
            if (!tx.isMature()) {
                continue;
            }
            for (TransactionOutput output : tx.getOutputs()) {
                if (!output.isAvailableForSpending()) continue;
                if (!output.isMine(this)) continue;
                gathered.add(output);
                valueGathered = valueGathered.add(output.getValue());
            }
            if (valueGathered.compareTo(total) >= 0)
                break;
        }
        // Can we afford this?
        if (valueGathered.compareTo(total) < 0) {
            // If there are insufficient unspent coins, see if there are any pending coins that have change-back-to-self 
            // and that have been seen by two or more peers. These are eligible for spending ("The Boomerang Rule")
            for (Transaction tx : pending.values()) {
                // Do not try and spend coinbases that were mined too recently, the protocol forbids it.
                if (!tx.isMature()) {
                    continue;
                }
                
                if (transactionSpendsFromThisWalletAndHasBoomerangedBack(tx)) {
                    for (TransactionOutput output : tx.getOutputs()) {
                        if (!output.isAvailableForSpending())
                            continue;
                        if (!output.isMine(this))
                            continue;

                        gathered.add(output);
                        valueGathered = valueGathered.add(output.getValue());
                    }
                    if (valueGathered.compareTo(total) >= 0)
                        break;
                }
            }   
            
            if (valueGathered.compareTo(total) < 0) {
                // Still not enough funds.
                log.info("Insufficient value in wallet for send, missing "
                        + bitcoinValueToFriendlyString(nanocoins.subtract(valueGathered)));
                // TODO: Should throw an exception here.
                return false;
            }
        }
        checkState(gathered.size() > 0);
        sendTx.getConfidence().setConfidenceType(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
        BigInteger change = valueGathered.subtract(total);
        if (change.compareTo(BigInteger.ZERO) > 0) {
            // The value of the inputs is greater than what we want to send. Just like in real life then,
            // we need to take back some coins ... this is called "change". Add another output that sends the change
            // back to us.
            log.info("  with " + bitcoinValueToFriendlyString(change) + " coins change");
            sendTx.addOutput(new TransactionOutput(params, sendTx, change, changeAddress));
        }
        for (TransactionOutput output : gathered) {
            sendTx.addInput(output);
        }

        // Now sign the inputs, thus proving that we are entitled to redeem the connected outputs.
        try {
            sendTx.signInputs(Transaction.SigHash.ALL, this, aesKey);
        } catch (ScriptException e) {
            // If this happens it means an output script in a wallet tx could not be understood. That should never
            // happen, if it does it means the wallet has got into an inconsistent state.
            throw new RuntimeException(e);
        } catch (KeyCrypterException kce) {
            // This can occur if the key being used is encrypted.
            throw new RuntimeException(kce);
        }

        // keep a track of the date the tx was created (used in MultiBitService
        // to work out the block it appears in)
        sendTx.setUpdateTime(new Date());

        log.info("  completed {}", sendTx.getHashAsString());
        return true;
    }

    private boolean transactionSpendsFromThisWalletAndHasBoomerangedBack(Transaction tx) {
        boolean seenByEnoughPeers = false;
        TransactionConfidence confidence = tx.getConfidence();
        if (confidence != null && confidence.getBroadcastBy() != null 
                && confidence.getBroadcastBy().size() >= MINIMUM_NUMBER_OF_PEERS_A_TRANSACTION_IS_SEEN_BY_FOR_SPEND) {
            seenByEnoughPeers = true;
        }
        
        if (!seenByEnoughPeers) return false;
        
        boolean wasSentFromMyWallet = true;
        // Check all transaction inputs are from your own wallet
        for (TransactionInput input : tx.getInputs()) {
            TransactionOutPoint outPoint = input.getOutpoint();
            try {
                TransactionOutput transactionOutput = outPoint.getConnectedOutput();
                if (transactionOutput == null) {
                    wasSentFromMyWallet = false;
                    break;
                }
                ECKey ecKey = outPoint.getConnectedKey(this);
                // if no ecKey was found then the transaction uses a transaction output from somewhere else
                if (ecKey == null) {
                    wasSentFromMyWallet = false;
                    break;
                }
            } catch (ScriptException e) {
                wasSentFromMyWallet = false;
                break;
            }
        }
        
        if (!wasSentFromMyWallet) return false;
        
        // Transaction is both from out wallet and has boomeranged back.
        return true;
    }
    
    /**
     * Takes a transaction with arbitrary outputs, gathers the necessary inputs for spending, and signs it.
     * Change goes to {@link Wallet#getChangeAddress()}
     * @param sendTx           The transaction to complete
     * @param fee              The fee to include
     * @return False if we cannot afford this send, true otherwise
     * @throws KeyCrypterException 
     * @throws IllegalStateException 
     */
    public synchronized boolean completeTx(Transaction sendTx, BigInteger fee, KeyParameter aesKey) throws IllegalStateException, KeyCrypterException {
        return completeTx(sendTx, getChangeAddress(), fee, aesKey);
    }

    synchronized Address getChangeAddress() {
        // For now let's just pick the first key in our keychain. In future we might want to do something else to
        // give the user better privacy here, eg in incognito mode.
        checkState(keychain.size() > 0, "Can't send value without an address to use for receiving change");
        ECKey first = keychain.get(0);
        return first.toAddress(params);
    }

    /**
     * Adds the given ECKey to the wallet. There is currently no way to delete keys (that would result in coin loss).
     * If {@link Wallet#autosaveToFile(java.io.File, long, java.util.concurrent.TimeUnit, com.google.bitcoin.core.Wallet.AutosaveEventListener)}
     * has been called, triggers an auto save bypassing the normal coalescing delay and event handlers.
     * If the key already exists in the wallet, does nothing and returns false.
     * @throws KeyCrypterException 
     */
    public synchronized boolean addKey(final ECKey key) throws KeyCrypterException {
        return addKeys(Lists.newArrayList(key)) == 1;
    }

    /**
     * Adds the given keys to the wallet. There is currently no way to delete keys (that would result in coin loss).
     * If {@link Wallet#autosaveToFile(java.io.File, long, java.util.concurrent.TimeUnit, com.google.bitcoin.core.Wallet.AutosaveEventListener)}
     * has been called, triggers an auto save bypassing the normal coalescing delay and event handlers.
     * Returns the number of keys added, after duplicates are ignored. The onKeyAdded event will be called for each key
     * in the list that was not already present.
     */
    public synchronized int addKeys(final List<ECKey> keys) throws KeyCrypterException {
        // TODO: Consider making keys a sorted list or hashset so membership testing is faster.
        int added = 0;
        for (final ECKey key : keys) {
            if (keychain.contains(key)) continue;

            // If the key has no KeyCrypter then the Wallet's is set into it.
            if (key.getKeyCrypter() == null) {
                key.setKeyCrypter(getKeyCrypter());
            } else {
                // If the key has an KeyCrypter that does not match the Wallet's then an exception is thrown.
                // This is done because only one KeyCrypter is stored per Wallet and hence the keys must be homogenous.
                if (!key.getKeyCrypter().equals(getKeyCrypter())) {
                    throw new KeyCrypterException("Cannot add key " + key.toString() + " because the KeyCrypter does not match the wallets");
                }
            }

            keychain.add(key);
            EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
                @Override
                public void invoke(WalletEventListener listener) {
                    listener.onKeyAdded(key);
                }
            });
            added++;
        }
        return added;
    }

    /**
     * Locates a keypair from the keychain given the hash of the public key. This is needed when finding out which
     * key we need to use to redeem a transaction output.
     *
     * @return ECKey object or null if no such key was found.
     */
    public synchronized ECKey findKeyFromPubHash(byte[] pubkeyHash) {
        for (ECKey key : keychain) {
            if (Arrays.equals(key.getPubKeyHash(), pubkeyHash)) return key;
        }
        return null;
    }

    /** Returns true if the given key is in the wallet, false otherwise. Currently an O(N) operation. */
    public boolean hasKey(ECKey key) {
        return keychain.contains(key);
    }

    /**
     * Returns true if this wallet contains a public key which hashes to the given hash.
     */
    public synchronized boolean isPubKeyHashMine(byte[] pubkeyHash) {
        return findKeyFromPubHash(pubkeyHash) != null;
    }

    /**
     * Locates a keypair from the keychain given the raw public key bytes.
     *
     * @return ECKey or null if no such key was found.
     */
    public synchronized ECKey findKeyFromPubKey(byte[] pubkey) {
        for (ECKey key : keychain) {
            if (Arrays.equals(key.getPubKey(), pubkey)) return key;
        }
        return null;
    }

    /**
     * Returns true if this wallet contains a keypair with the given public key.
     */
    public synchronized boolean isPubKeyMine(byte[] pubkey) {
        return findKeyFromPubKey(pubkey) != null;
    }

    /**
     * It's possible to calculate a wallets balance from multiple points of view. This enum selects which
     * getBalance() should use.<p>
     * <p/>
     * Consider a real-world example: you buy a snack costing $5 but you only have a $10 bill. At the start you have
     * $10 viewed from every possible angle. After you order the snack you hand over your $10 bill. From the
     * perspective of your wallet you have zero dollars (AVAILABLE). But you know in a few seconds the shopkeeper
     * will give you back $5 change so most people in practice would say they have $5 (ESTIMATED).<p>
     */
    public enum BalanceType {
        /**
         * Balance calculated assuming all pending transactions are in fact included into the best chain by miners.
         * This is the right balance to show in user interfaces.
         */
        ESTIMATED,

        /**
         * Balance that can be safely used to create new spends. This is all confirmed unspent outputs minus the ones
         * spent by pending transactions, but not including the outputs of those pending transactions.
         */
        AVAILABLE,
        
        /**
         * Balance that can be safely used to create new spends. This includes all available and any change that
         * has been been by MINIMUM_NUMBER_OF_PEERS_A_TRANSACTION_IS_SEEN_BY_FOR_SPEND or more peers.
         */
        AVAILABLE_WITH_BOOMERANG_CHANGE
    }

    /**
     * Returns the AVAILABLE balance of this wallet. See {@link BalanceType#AVAILABLE} for details on what this
     * means.<p>
     * <p/>
     * Note: the estimated balance is usually the one you want to show to the end user - however attempting to
     * actually spend these coins may result in temporary failure. This method returns how much you can safely
     * provide to {@link Wallet#createSend(Address, java.math.BigInteger)}.
     */
    public synchronized BigInteger getBalance() {
        return getBalance(BalanceType.AVAILABLE);
    }

    /**
     * Returns the balance of this wallet as calculated by the provided balanceType.
     */
    public synchronized BigInteger getBalance(BalanceType balanceType) {
        BigInteger available = BigInteger.ZERO;
        for (Transaction tx : unspent.values()) {
            // For an 'available to spend' balance exclude coinbase transactions that have not yet matured.
            if ((balanceType == BalanceType.AVAILABLE || balanceType == BalanceType.AVAILABLE_WITH_BOOMERANG_CHANGE) && !tx.isMature()) {
                continue;
            }

            for (TransactionOutput output : tx.getOutputs()) {
                if (!output.isMine(this)) continue;
                if (!output.isAvailableForSpending()) continue;
                available = available.add(output.getValue());
            }
        }
        if (balanceType == BalanceType.AVAILABLE)
            return available;
        checkState(balanceType == BalanceType.ESTIMATED || balanceType == BalanceType.AVAILABLE_WITH_BOOMERANG_CHANGE);        
        if (balanceType == BalanceType.ESTIMATED) {
            // Now add back all the pending outputs to assume the transaction goes through.
            BigInteger estimated = available;

            for (Transaction tx : pending.values()) {
                for (TransactionOutput output : tx.getOutputs()) {
                    if (!output.isMine(this)) continue;
                    if (!output.isAvailableForSpending()) continue;
                    estimated = estimated.add(output.getValue());
                }
            }

            return estimated;
        } else {
            BigInteger availableWithBoomerang = available;

            // If transactions send from your own wallet then change is spendable after the transaction has been seen by
            // MINIMUM_NUMBER_OF_PEERS_A_TRANSACTION_IS_SEEN_BY_FOR_SPEND
            for (Transaction tx : pending.values()) {
                if (transactionSpendsFromThisWalletAndHasBoomerangedBack(tx)) {
                    // Transaction is both from out wallet and has boomeranged back.
                    for (TransactionOutput output : tx.getOutputs()) {
                        if (!output.isMine(this))
                            continue;
                        if (!output.isAvailableForSpending())
                            continue;
                        availableWithBoomerang = availableWithBoomerang.add(output.getValue());
                    }
                }
            }
            return availableWithBoomerang;
        }
    }

    @Override
    public synchronized String toString() {
        return toString(false);
    }

    public synchronized String toString(boolean includePrivateKeys) {
        StringBuilder builder = new StringBuilder();
        builder.append(String.format("Wallet containing %s BTC in:%n", bitcoinValueToFriendlyString(getBalance())));
        builder.append(String.format("  %d unspent transactions%n", unspent.size()));
        builder.append(String.format("  %d spent transactions%n", spent.size()));
        builder.append(String.format("  %d pending transactions%n", pending.size()));
        builder.append(String.format("  %d inactive transactions%n", inactive.size()));
        builder.append(String.format("  %d dead transactions%n", dead.size()));
        builder.append(String.format("Last seen best block: %s%n", getLastBlockSeenHash()));
        // Do the keys.
        builder.append("\nKeys:\n");
        for (ECKey key : keychain) {
            builder.append("  addr:");
            builder.append(key.toAddress(params));
            builder.append(" ");
            builder.append(includePrivateKeys ? key.toStringWithPrivate() : key.toString());
            builder.append("\n");
        }
        // Print the transactions themselves
        if (unspent.size() > 0) {
            builder.append("\nUNSPENT:\n");
            toStringHelper(builder, unspent);
        }
        if (spent.size() > 0) {
            builder.append("\nSPENT:\n");
            toStringHelper(builder, spent);
        }
        if (pending.size() > 0) {
            builder.append("\nPENDING:\n");
            toStringHelper(builder, pending);
        }
        if (inactive.size() > 0) {
            builder.append("\nINACTIVE:\n");
            toStringHelper(builder, inactive);
        }
        if (dead.size() > 0) {
            builder.append("\nDEAD:\n");
            toStringHelper(builder, dead);
        }

        // Add the keyCrypter so that any setup parameters are in the wallet toString.
        if (this.keyCrypter != null) {
            builder.append("\n KeyCrypter: " + keyCrypter.toString());
        }
        return builder.toString();
    }

    private void toStringHelper(StringBuilder builder, Map<Sha256Hash, Transaction> transactionMap) {
        for (Transaction tx : transactionMap.values()) {
            try {
                builder.append("Sends ");
                builder.append(Utils.bitcoinValueToFriendlyString(tx.getValueSentFromMe(this)));
                builder.append(" and receives ");
                builder.append(Utils.bitcoinValueToFriendlyString(tx.getValueSentToMe(this)));
                builder.append(", total value ");
                builder.append(Utils.bitcoinValueToFriendlyString(tx.getValue(this)));
                builder.append(".\n");
            } catch (ScriptException e) {
                // Ignore and don't print this line.
            }
            builder.append(tx);
        }
    }

    /**
     * Called by the {@link BlockChain} when the best chain (representing total work done) has changed. In this case,
     * we need to go through our transactions and find out if any have become invalid. It's possible for our balance
     * to go down in this case: money we thought we had can suddenly vanish if the rest of the network agrees it
     * should be so.<p>
     *
     * The oldBlocks/newBlocks lists are ordered height-wise from top first to bottom last.
     */
    synchronized void reorganize(StoredBlock splitPoint, List<StoredBlock> oldBlocks, List<StoredBlock> newBlocks) throws VerificationException {
        // This runs on any peer thread with the block chain synchronized.
        //
        // The reorganize functionality of the wallet is tested in ChainSplitTests.
        //
        // For each transaction we track which blocks they appeared in. Once a re-org takes place we have to find all
        // transactions in the old branch, all transactions in the new branch and find the difference of those sets.
        //
        // receive() has been called on the block that is triggering the re-org before this is called.

        List<Sha256Hash> oldBlockHashes = new ArrayList<Sha256Hash>(oldBlocks.size());
        List<Sha256Hash> newBlockHashes = new ArrayList<Sha256Hash>(newBlocks.size());
        log.info("Old part of chain (top to bottom):");
        for (StoredBlock b : oldBlocks) {
            log.info("  {}", b.getHeader().getHashAsString());
            oldBlockHashes.add(b.getHeader().getHash());
        }
        log.info("New part of chain (top to bottom):");
        for (StoredBlock b : newBlocks) {
            log.info("  {}", b.getHeader().getHashAsString());
            newBlockHashes.add(b.getHeader().getHash());
        }

        // Transactions that appear in the old chain segment.
        Map<Sha256Hash, Transaction> oldChainTransactions = new HashMap<Sha256Hash, Transaction>();
        // Transactions that appear in the old chain segment and NOT the new chain segment.
        Map<Sha256Hash, Transaction> onlyOldChainTransactions = new HashMap<Sha256Hash, Transaction>();
        // Transactions that appear in the new chain segment.
        Map<Sha256Hash, Transaction> newChainTransactions = new HashMap<Sha256Hash, Transaction>();
        // Transactions that don't appear in either the new or the old section, ie, the shared trunk.
        Map<Sha256Hash, Transaction> commonChainTransactions = new HashMap<Sha256Hash, Transaction>();

        Map<Sha256Hash, Transaction> all = new HashMap<Sha256Hash, Transaction>();
        all.putAll(unspent);
        all.putAll(spent);
        all.putAll(inactive);

        // Dead coinbase transactions are potentially resurrected so added to the list of tx to process.
        for (Transaction tx : dead.values()) {
            if (tx.isCoinBase()) {
                all.put(tx.getHash(), tx);
            }
        }

        for (Transaction tx : all.values()) {
            Collection<Sha256Hash> appearsIn = tx.getAppearsInHashes();
            checkNotNull(appearsIn);
            // If the set of blocks this transaction appears in is disjoint with one of the chain segments it means
            // the transaction was never incorporated by a miner into that side of the chain.
            boolean inOldSection = !Collections.disjoint(appearsIn, oldBlockHashes);
            boolean inNewSection = !Collections.disjoint(appearsIn, newBlockHashes);
            boolean inCommonSection = !inNewSection && !inOldSection;

            if (inCommonSection) {
                boolean alreadyPresent = commonChainTransactions.put(tx.getHash(), tx) != null;
                checkState(!alreadyPresent, "Transaction appears twice in common chain segment");
            } else {
                if (inOldSection) {
                    boolean alreadyPresent = oldChainTransactions.put(tx.getHash(), tx) != null;
                    checkState(!alreadyPresent, "Transaction appears twice in old chain segment");
                    if (!inNewSection) {
                        alreadyPresent = onlyOldChainTransactions.put(tx.getHash(), tx) != null;
                        checkState(!alreadyPresent, "Transaction appears twice in only-old map");
                    }
                }
                if (inNewSection) {
                    boolean alreadyPresent = newChainTransactions.put(tx.getHash(), tx) != null;
                    checkState(!alreadyPresent, "Transaction appears twice in new chain segment");
                }
            }
        }

        // If there is no difference it means we have nothing we need to do and the user does not care.
        boolean affectedUs = !oldChainTransactions.equals(newChainTransactions);
        log.info(affectedUs ? "Re-org affected our transactions" : "Re-org had no effect on our transactions");
        if (!affectedUs) return;

        // Avoid spuriously informing the user of wallet changes whilst we're re-organizing. This also prevents the
        // user from modifying wallet contents (eg, trying to spend) whilst we're in the middle of the process.
        onWalletChangedSuppressions++;

        // For simplicity we will reprocess every transaction to ensure it's in the right bucket and has the right
        // connections. Attempting to update each one with minimal work is possible but complex and was leading to
        // edge cases that were hard to fix. As re-orgs are rare the amount of work this implies should be manageable
        // unless the user has an enormous wallet. As an optimization fully spent transactions buried deeper than
        // 1000 blocks could be put into yet another bucket which we never touch and assume re-orgs cannot affect.

        for (Transaction tx : onlyOldChainTransactions.values()) log.info("  Only Old: {}", tx.getHashAsString());
        for (Transaction tx : oldChainTransactions.values()) log.info("  Old: {}", tx.getHashAsString());
        for (Transaction tx : newChainTransactions.values()) log.info("  New: {}", tx.getHashAsString());

        // Break all the existing connections.
        for (Transaction tx : all.values())
            tx.disconnectInputs();
        for (Transaction tx : pending.values())
            tx.disconnectInputs();
        // Reconnect the transactions in the common part of the chain.
        for (Transaction tx : commonChainTransactions.values()) {
            TransactionInput badInput = tx.connectForReorganize(all);
            checkState(badInput == null, "Failed to connect %s, %s", tx.getHashAsString(),
                       badInput == null ? "" : badInput.toString());
        }
        // Recalculate the unspent/spent buckets for the transactions the re-org did not affect.
        log.info("Moving transactions");
        unspent.clear();
        spent.clear();
        inactive.clear();
        for (Transaction tx : commonChainTransactions.values()) {
            int unspentOutputs = 0;
            for (TransactionOutput output : tx.getOutputs()) {
                if (output.isAvailableForSpending() && output.isMine(this)) unspentOutputs++;
            }
            if (unspentOutputs > 0) {
                log.info("  TX {} ->unspent", tx.getHashAsString());
                unspent.put(tx.getHash(), tx);
            } else {
                log.info("  TX {} ->spent", tx.getHashAsString());
                spent.put(tx.getHash(), tx);
            }
        }

        // Inform all transactions that exist only in the old chain that they have moved, so they can update confidence
        // and timestamps. Transactions will be told they're on the new best chain when the blocks are replayed.
        for (Transaction tx : onlyOldChainTransactions.values()) {
            tx.notifyNotOnBestChain();

            // Kill any coinbase transactions that are only in the old chain.
            // These transactions are no longer valid.
            if (tx.isCoinBase()) {
                // Move the transaction to the dead pool.
                if (unspent.containsKey(tx.getHash())) {
                    log.info("  coinbase tx {} unspent->dead", tx.getHashAsString());
                    unspent.remove(tx.getHash());
                } else if (spent.containsKey(tx.getHash())) {
                    log.info("  coinbase tx {} spent->dead", tx.getHashAsString());
                    // TODO Remove any dependent child transactions of the just removed coinbase transaction.
                    spent.remove(tx.getHash());
                }
                dead.put(tx.getHash(), tx);

                // Set transaction confidence to dead and notify listeners.
                tx.getConfidence().setConfidenceType(ConfidenceType.DEAD);
            }
        }

        // Now replay the act of receiving the blocks that were previously in a side chain. This will:
        //   - Move any transactions that were pending and are now accepted into the right bucket.
        //   - Connect the newly active transactions.

        Collections.reverse(newBlocks);  // Need bottom-to-top but we get top-to-bottom.

        // The old blocks have contributed to the depth and work done for all the transactions in the
        // wallet that are in blocks up to and including the chain split block.
        // The total depth and work done is calculated here and then subtracted from the appropriate transactions.
        int depthToSubtract = oldBlocks.size();

        BigInteger workDoneToSubtract = BigInteger.ZERO;
        for (StoredBlock b : oldBlocks) {
            workDoneToSubtract = workDoneToSubtract.add(b.getHeader().getWork());
        }
        log.info("DepthToSubtract = " + depthToSubtract + ", workDoneToSubtract = " + workDoneToSubtract);

        // Remove depthToSubtract and workDoneToSubtract from all transactions in the wallet except for pending and inactive
        // (i.e. the transactions in the two chains of blocks we are reorganising).
        subtractDepthAndWorkDone(depthToSubtract, workDoneToSubtract, spent.values());
        subtractDepthAndWorkDone(depthToSubtract, workDoneToSubtract, unspent.values());
        subtractDepthAndWorkDone(depthToSubtract, workDoneToSubtract, dead.values());

        // The effective last seen block is now the split point so set the lastSeenBlockHash.
        setLastBlockSeenHash(splitPoint.getHeader().getHash());

        for (StoredBlock b : newBlocks) {
            log.info("Replaying block {}", b.getHeader().getHashAsString());
            // Replay means: find the transactions that should be in that block, send them to the wallet, inform of
            // new best block, repeat.
            Set<Transaction> txns = new HashSet<Transaction>();
            Sha256Hash blockHash = b.getHeader().getHash();
            for (Transaction tx : newChainTransactions.values()) {
                if (tx.getAppearsInHashes().contains(blockHash)) {
                    txns.add(tx);
                    log.info("  containing tx {}", tx.getHashAsString());
                }
            }

            if (!txns.isEmpty()) {
                // Add the transactions to the new blocks.
                for (Transaction t : txns) {
                    try {
                        receive(t, b, BlockChain.NewBlockType.BEST_CHAIN, true);
                    } catch (ScriptException e) {
                        throw new RuntimeException(e);  // Cannot happen as these blocks were already verified.
                    }
                }
            }
            notifyNewBestBlock(b.getHeader());
        }

        // Find the transactions that didn't make it into the new chain yet. For each input, try to connect it to the
        // transactions that are in {spent,unspent,pending}. Check the status of each input. For inactive
        // transactions that only send us money, we put them into the inactive pool where they sit around waiting for
        // another re-org or re-inclusion into the main chain. For inactive transactions where we spent money we must
        // put them back into the pending pool if we can reconnect them, so we don't create a double spend whilst the
        // network heals itself.
        Map<Sha256Hash, Transaction> pool = new HashMap<Sha256Hash, Transaction>();
        pool.putAll(unspent);
        pool.putAll(spent);
        pool.putAll(pending);
        Map<Sha256Hash, Transaction> toReprocess = new HashMap<Sha256Hash, Transaction>();
        toReprocess.putAll(onlyOldChainTransactions);
        toReprocess.putAll(pending);
        log.info("Reprocessing transactions not in new best chain:");
        // Note, we must reprocess dead transactions first. The reason is that if there is a double spend across
        // chains from our own coins we get a complicated situation:
        //
        // 1) We switch to a new chain (B) that contains a double spend overriding a pending transaction. The
        //    pending transaction goes dead.
        // 2) We switch BACK to the first chain (A). The dead transaction must go pending again.
        // 3) We resurrect the transactions that were in chain (B) and assume the miners will start work on putting them
        //    in to the chain, but it's not possible because it's a double spend. So now that transaction must become
        //    dead instead of pending.
        //
        // This only occurs when we are double spending our own coins.
        for (Transaction tx : dead.values()) {
            reprocessUnincludedTxAfterReorg(pool, tx);
        }
        for (Transaction tx : toReprocess.values()) {
            reprocessUnincludedTxAfterReorg(pool, tx);
        }

        log.info("post-reorg balance is {}", Utils.bitcoinValueToFriendlyString(getBalance()));

        // Inform event listeners that a re-org took place. They should save the wallet at this point.
        EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
            @Override
            public void invoke(WalletEventListener listener) {
                listener.onReorganize(Wallet.this);
            }
        });
        onWalletChangedSuppressions--;
        invokeOnWalletChanged();
        checkState(isConsistent());
    }

    /**
     * Subtract the supplied depth and work done from the given transactions.
     */
    synchronized private void subtractDepthAndWorkDone(int depthToSubtract, BigInteger workDoneToSubtract, Collection<Transaction> transactions) {
        for (Transaction tx : transactions) {
            if (tx.getConfidence().getConfidenceType() == ConfidenceType.BUILDING) {
                tx.getConfidence().setDepthInBlocks(tx.getConfidence().getDepthInBlocks() - depthToSubtract);
                tx.getConfidence().setWorkDone(tx.getConfidence().getWorkDone().subtract(workDoneToSubtract));
            }
        }
    }

    private void reprocessUnincludedTxAfterReorg(Map<Sha256Hash, Transaction> pool, Transaction tx) {
        log.info("TX {}", tx.getHashAsString() + ", confidence = " + tx.getConfidence().getConfidenceType().name());

        boolean isDeadCoinbase = tx.isCoinBase() && ConfidenceType.DEAD == tx.getConfidence().getConfidenceType();

        // Dead coinbase transactions on a side chain stay dead.
        if (isDeadCoinbase) {
            return;
        }

        int numInputs = tx.getInputs().size();
        int noSuchTx = 0;
        int success = 0;
        boolean isDead = false;
        // The transactions that we connected inputs to, so we can go back later and move them into the right
        // bucket if all their outputs got spent.
        Set<Transaction> connectedTransactions = new TreeSet<Transaction>();
        for (TransactionInput input : tx.getInputs()) {
            TransactionInput.ConnectionResult result = input.connect(pool, TransactionInput.ConnectMode.ABORT_ON_CONFLICT);
            if (result == TransactionInput.ConnectionResult.SUCCESS) {
                success++;
                TransactionOutput connectedOutput = checkNotNull(input.getConnectedOutput(pool));
                connectedTransactions.add(checkNotNull(connectedOutput.parentTransaction));
            } else if (result == TransactionInput.ConnectionResult.NO_SUCH_TX) {
                noSuchTx++;
            } else if (result == TransactionInput.ConnectionResult.ALREADY_SPENT) {
                isDead = true;
                // This transaction was replaced by a double spend on the new chain. Did you just reverse
                // your own transaction? I hope not!!
                log.info("   ->dead, will not confirm now unless there's another re-org", tx.getHashAsString());
                TransactionOutput doubleSpent = input.getConnectedOutput(pool);
                Transaction replacement = doubleSpent.getSpentBy().getParentTransaction();
                dead.put(tx.getHash(), tx);
                pending.remove(tx.getHash());
                // This updates the tx confidence type automatically.
                tx.getConfidence().setOverridingTransaction(replacement);
                break;
            }
        }
        if (isDead) return;

        // If all inputs do not appear in this wallet move to inactive.
        if (noSuchTx == numInputs) {
            log.info("   ->inactive", tx.getHashAsString() + ", confidence = " + tx.getConfidence().getConfidenceType().name());
            inactive.put(tx.getHash(), tx);
            dead.remove(tx.getHash());
        } else if (success == numInputs - noSuchTx) {
            // All inputs are either valid for spending or don't come from us. Miners are trying to reinclude it.
            log.info("   ->pending", tx.getHashAsString() + ", confidence = " + tx.getConfidence().getConfidenceType().name());
            pending.put(tx.getHash(), tx);
            dead.remove(tx.getHash());
        }

        // The act of re-connecting this un-included transaction may have caused other transactions to become fully
        // spent so move them into the right bucket here to keep performance good.
        for (Transaction maybeSpent : connectedTransactions) {
            maybeMoveTxToSpent(maybeSpent, "reorg");
        }
    }

    private void invokeOnTransactionConfidenceChanged(final Transaction tx) {
        EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
            @Override
            public void invoke(WalletEventListener listener) {
                listener.onTransactionConfidenceChanged(Wallet.this, tx);
            }
        });
    }

    private int onWalletChangedSuppressions;
    private synchronized void invokeOnWalletChanged() {
        // Don't invoke the callback in some circumstances, eg, whilst we are re-organizing or fiddling with
        // transactions due to a new block arriving. It will be called later instead.
        Preconditions.checkState(onWalletChangedSuppressions >= 0);
        if (onWalletChangedSuppressions > 0) return;
        // Call with the wallet locked.
        EventListenerInvoker.invoke(eventListeners, new EventListenerInvoker<WalletEventListener>() {
            @Override
            public void invoke(WalletEventListener listener) {
                listener.onWalletChanged(Wallet.this);
            }
        });
    }

    /**
     * Returns an immutable view of the transactions currently waiting for network confirmations.
     */
    public synchronized Collection<Transaction> getPendingTransactions() {
        return Collections.unmodifiableCollection(pending.values());
    }

    /**
     * Returns the earliest creation time of the keys in this wallet, in seconds since the epoch, ie the min of 
     * {@link com.google.bitcoin.core.ECKey#getCreationTimeSeconds()}. This can return zero if at least one key does
     * not have that data (was created before key timestamping was implemented). <p>
     *     
     * This method is most often used in conjunction with {@link PeerGroup#setFastCatchupTimeSecs(long)} in order to
     * optimize chain download for new users of wallet apps. Backwards compatibility notice: if you get zero from this
     * method, you can instead use the time of the first release of your software, as it's guaranteed no users will
     * have wallets pre-dating this time. <p>
     * 
     * If there are no keys in the wallet, the current time is returned.
     */
    public synchronized long getEarliestKeyCreationTime() {
        if (keychain.size() == 0) {
            return Utils.now().getTime() / 1000;
        }
        long earliestTime = Long.MAX_VALUE;
        for (ECKey key : keychain) {
            earliestTime = Math.min(key.getCreationTimeSeconds(), earliestTime);
        }
        return earliestTime;
    }
    
    // This object is used to receive events from a Peer or PeerGroup. Currently it is only used to receive
    // transactions. Note that it does NOT pay attention to block message because they will be received from the
    // BlockChain object along with extra data we need for correct handling of re-orgs.
    private transient PeerEventListener peerEventListener;

    /**
     * The returned object can be used to connect the wallet to a {@link Peer} or {@link PeerGroup} in order to
     * receive and process blocks and transactions.
     */
    public synchronized PeerEventListener getPeerEventListener() {
        if (peerEventListener == null) {
            // Instantiate here to avoid issues with wallets resurrected from serialized copies.
            peerEventListener = new AbstractPeerEventListener() {
                @Override
                public void onTransaction(Peer peer, Transaction t) {
                    // Runs locked on a peer thread.
                    try {
                        receivePending(t);
                    } catch (VerificationException e) {
                        log.warn("Received broadcast transaction that does not validate: {}", t);
                        log.warn("VerificationException caught", e);
                    }
                }
            };
        }
        return peerEventListener;
    }

    public Sha256Hash getLastBlockSeenHash() {
        return lastBlockSeenHash;
    }

    public void setLastBlockSeenHash(Sha256Hash lastBlockSeenHash) {
        this.lastBlockSeenHash = lastBlockSeenHash;
    }
    
    public Collection<ECKey> getKeychain() {
        return keychain;
    }

    /**
     * Deletes transactions which appeared after a certain date
     */
    public synchronized void clearTransactions(Date fromDate) {
        if (fromDate == null) {
            unspent.clear();
            spent.clear();
            pending.clear();
            inactive.clear();
            dead.clear();
        } else {
            removeEntriesAfterDate(unspent, fromDate);
            removeEntriesAfterDate(spent, fromDate);
            removeEntriesAfterDate(pending, fromDate);
            removeEntriesAfterDate(inactive, fromDate);
            removeEntriesAfterDate(dead, fromDate);
        }
    }

    private void removeEntriesAfterDate(Map<Sha256Hash, Transaction> pool, Date fromDate) {
        log.debug("Wallet#removeEntriesAfterDate - Removing transactions later than " + fromDate.toString());
        Set<Entry<Sha256Hash, Transaction>> loopEntries = pool.entrySet();
        Iterator<Entry<Sha256Hash, Transaction>> iterator = loopEntries.iterator();
        while(iterator.hasNext()) {
            Entry<Sha256Hash, Transaction> member = iterator.next();
            if (member.getValue() != null) {
                Date updateTime = member.getValue().getUpdateTime();
                if (updateTime != null && updateTime.after(fromDate)) {
                    iterator.remove();
                    log.debug("Wallet#removeEntriesAfterDate - Removed tx.1 " + member.getValue());
                    continue;
                }
                
                // if no updateTime remove them
                if (updateTime == null || updateTime.getTime() == 0) {
                    iterator.remove();
                    log.debug("Removed tx.2 " + member.getValue());
                    continue;                    
                }
            }
        }
    }

    public EncryptionType getEncryptionType() {
        return encryptionType;
    }

    public void setEncryptionType(EncryptionType encryptionType) {
        this.encryptionType = encryptionType;
    }

    /**
     * Encrypt the wallet with the supplied AES key.
     * @param aesKey AES key to use (normally created using KeyCrypter#deriveKey)
     * @throws KeyCrypterException 
     */
    synchronized public void encrypt(KeyParameter aesKey) throws KeyCrypterException {
        if (getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
            throw new WalletIsAlreadyEncryptedException("Wallet is already encrypted");
        }

        // Create a new arraylist that will contain the encrypted keys
        ArrayList<ECKey> encryptedKeyChain = new ArrayList<ECKey>();

        for (ECKey key : keychain) {
            if (key.isEncrypted()) {
                throw new WalletIsAlreadyEncryptedException("Key '" + key.toString() + "' is already encrypted.");
            }

            // Clone the key before encrypting.
            // (note that only the EncryptedPrivateKey is deep copied).
            ECKey clonedECKey = new ECKey(key.getPrivKeyBytes(), new EncryptedPrivateKey(key.getEncryptedPrivateKey()),
                    key.getPubKey(), keyCrypter);
            clonedECKey.encrypt(aesKey);
            encryptedKeyChain.add(clonedECKey);
        }

        // Replace the old keychain with the encrypted one.
        keychain = encryptedKeyChain;
        setEncryptionType(EncryptionType.ENCRYPTED_SCRYPT_AES);
    }

    /**
     * Decrypt the wallet with the supplied AES key.
     * 
     * @param aesKey AES key to use (normally created using KeyCrypter#deriveKey)
     * @throws KeyCrypterException 
     */
    synchronized public void decrypt(KeyParameter aesKey) throws KeyCrypterException {
        if (getEncryptionType() == EncryptionType.UNENCRYPTED) {
            throw new WalletIsAlreadyDecryptedException("Wallet is already decrypted");
        }

        // Create a new arraylist that will contain the decrypted keys
        ArrayList<ECKey> decryptedKeyChain = new ArrayList<ECKey>();

        for (ECKey key : keychain) {
            if (!key.isEncrypted()) {
                throw new WalletIsAlreadyDecryptedException("Key '" + key.toString() + "' is already decrypted.");
            }

            // Clone the key before decrypting.
            // (note that only the EncryptedPrivateKey is deep copied).
            ECKey clonedECKey = new ECKey(new EncryptedPrivateKey(key.getEncryptedPrivateKey()), key.getPubKey(),
                    keyCrypter);
            clonedECKey.decrypt(aesKey);
            decryptedKeyChain.add(clonedECKey);
        }

        // Replace the old keychain with the unencrypted one.
        keychain = decryptedKeyChain;
        setEncryptionType(EncryptionType.UNENCRYPTED);
    }

    /**
     *  Check whether the password can decrypt the first key in the wallet.
     *  @throws KeyCrypterException 
     *
     *  @returns boolean True if password supplied can decrypt the first private key in the wallet, false otherwise.
     */
    public boolean checkPasswordCanDecryptFirstPrivateKey(char[] password) throws KeyCrypterException {
        if (keyCrypter == null) {
            // The password cannot decrypt anything as the keyCrypter is null.
            return false;
        }
        return checkAESKeyCanDecryptFirstPrivateKey(keyCrypter.deriveKey(password));
    }

    /**
     *  Check whether the AES key can decrypt the first key in the wallet.
     *  This can be used to check the validity of an entered password.
     *
     *  @returns boolean True if AES key supplied can decrypt the first private key in the wallet, false otherwise.
     */
    public boolean checkAESKeyCanDecryptFirstPrivateKey(KeyParameter aesKey) {
        if (getKeychain() == null || getKeychain().size() == 0) {
            return false;
        }

        ECKey firstECKey = getKeychain().iterator().next();

        if (firstECKey != null && firstECKey.getEncryptedPrivateKey() != null) {
            try {
                EncryptedPrivateKey clonedPrivateKey  = new EncryptedPrivateKey(firstECKey.getEncryptedPrivateKey());
                keyCrypter.decrypt(clonedPrivateKey, aesKey);

                // Success.
                return true;
            } catch (KeyCrypterException ede) {
                // The AES key supplied is incorrect.
                return false;
            }
        }

        return false;
    }

    public KeyCrypter getKeyCrypter() {
        return keyCrypter;
    }

    public void setKeyCrypter(KeyCrypter keyCrypter) {
        this.keyCrypter = keyCrypter;
    }

    public WalletVersion getVersion() {
        return version;
    }

    public void setVersion(WalletVersion version) {
        this.version = version;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
