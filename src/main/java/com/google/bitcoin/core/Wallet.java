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
import java.util.Random;
import java.util.Set;

import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.multibit.IsMultiBitClass;
import org.multibit.store.MultiBitWalletProtobufSerializer;
import org.multibit.store.MultiBitWalletVersion;
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
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.ListenableFuture;

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
public class Wallet implements Serializable, BlockChainListener, IsMultiBitClass {
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

    private NetworkParameters params;

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
    // Whether or not to ignore nLockTime > 0 transactions that are received to the mempool.
    private boolean acceptTimeLockedTransactions;

    /** Represents the results of a {@link CoinSelector#select(java.math.BigInteger, java.util.LinkedList)}  operation */
    public static class CoinSelection {
        public BigInteger valueGathered;
        public List<TransactionOutput> gathered;
        public CoinSelection(BigInteger valueGathered, List<TransactionOutput> gathered) {
            this.valueGathered = valueGathered;
            this.gathered = gathered;
        }
    }

    /**
     * A CoinSelector is responsible for picking some outputs to spend, from the list of all spendable outputs. It
     * allows you to customize the policies for creation of transactions to suit your needs. The select operation
     * may return a {@link CoinSelection} that has a valueGathered lower than the requested target, if there's not
     * enough money in the wallet.
     */
    public interface CoinSelector {
        public CoinSelection select(BigInteger target, LinkedList<TransactionOutput> candidates);
    }

    public static class DefaultCoinSelector implements CoinSelector {
        public CoinSelection select(BigInteger biTarget, LinkedList<TransactionOutput> candidates) {
            long target = biTarget.longValue();
            long total = 0;
            LinkedList<TransactionOutput> selected = Lists.newLinkedList();
            // Sort the inputs by age so we use oldest first.
            // TODO: Consider changing the wallets internal format to track just outputs and keep them ordered.
            ArrayList<TransactionOutput> sortedOutputs = new ArrayList<TransactionOutput>(candidates);
            Collections.sort(sortedOutputs, new Comparator<TransactionOutput>() {
                public int compare(TransactionOutput a, TransactionOutput b) {
                    int depth1 = 0;
                    int depth2 = 0;
                    TransactionConfidence conf1 = a.parentTransaction.getConfidence();
                    TransactionConfidence conf2 = b.parentTransaction.getConfidence();
                    if (conf1.getConfidenceType() == ConfidenceType.BUILDING) depth1 = conf1.getDepthInBlocks();
                    if (conf2.getConfidenceType() == ConfidenceType.BUILDING) depth2 = conf2.getDepthInBlocks();
                    if (depth1 < depth2)
                        return 1;
                    else if (depth1 > depth2)
                        return -1;
                    // Their depths are equal (possibly pending) so sort by hash to ensure a total ordering.
                    BigInteger aHash = a.parentTransaction.getHash().toBigInteger();
                    BigInteger bHash = b.parentTransaction.getHash().toBigInteger();
                    return aHash.compareTo(bHash);
                }
            });
            // Now iterate over the sorted outputs until we have got as close to the target as possible or a little
            // bit over (excessive value will be change).
            for (TransactionOutput output : sortedOutputs) {
                if (total >= target) break;
                // Only pick chain-included transactions, or transactions that are ours and pending.
                TransactionConfidence confidence = output.parentTransaction.getConfidence();
                ConfidenceType type = confidence.getConfidenceType();
                boolean pending = type.equals(ConfidenceType.NOT_SEEN_IN_CHAIN) ||
                                  type.equals(ConfidenceType.NOT_IN_BEST_CHAIN);
                boolean confirmed = type.equals(ConfidenceType.BUILDING);
                if (!confirmed) {
                    // If the transaction is still pending ...
                    if (!pending) continue;
                    // And it was created by us ...
                    if (!confidence.getSource().equals(TransactionConfidence.Source.SELF)) continue;
                    // And it's been seen by the network and propagated ...
                    if (confidence.numBroadcastPeers() <= 1) continue;
                    // Then it's OK to select.
                }
                selected.add(output);
                total += output.getValue().longValue();
            }
            // Total may be lower than target here, if the given candidates were insufficient to create to requested
            // transaction.
            return new CoinSelection(BigInteger.valueOf(total), selected);
        }
    }

    private transient CoinSelector coinSelector = new DefaultCoinSelector();

    /**
     * The keyCrypter for the wallet. This specifies the algorithm used for encrypting and decrypting the private keys.
     * Null for an unencrypted wallet.
     */
    private KeyCrypter keyCrypter;

    /**
     * The wallet version. This can be used to track breaking changes in the wallet format.
     * You can also use it to detect wallets that come from the future (ie they contain features you
     * do not know how to deal with).
     */
    MultiBitWalletVersion version;

    /**
     * A description for the wallet.
     */
    String description;

    /**
     * Creates a new, empty wallet with no keys and no transactions. If you want to restore a wallet from disk instead,
     * see loadFromFile.
     */
    public Wallet(NetworkParameters params) {
        this(params, null);
    }

    /**
     * Create a wallet with a keyCrypter to use in encrypting and decrypting keys.
     */
    public Wallet(NetworkParameters params, KeyCrypter keyCrypter) {
        this.params = checkNotNull(params);
        this.keyCrypter = keyCrypter;
        keychain = new ArrayList<ECKey>();
        unspent = new HashMap<Sha256Hash, Transaction>();
        spent = new HashMap<Sha256Hash, Transaction>();
        inactive = new HashMap<Sha256Hash, Transaction>();
        pending = new HashMap<Sha256Hash, Transaction>();
        dead = new HashMap<Sha256Hash, Transaction>();
        createTransientState();
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
        acceptTimeLockedTransactions = false;
        coinSelector = new DefaultCoinSelector();
    }

    public NetworkParameters getNetworkParameters() {
        return params;
    }
    
    public void setNetworkParameters(NetworkParameters params) {
        this.params = params;
    }

    /**
     * Returns a snapshot of the keychain. This view is not live.
     */
    public synchronized Iterable<ECKey> getKeys() {
        return new ArrayList<ECKey>(keychain);
    }

    /**
     * Returns the number of keys in the keychain.
     */
    public synchronized int getKeychainSize() {
        return keychain.size();
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
     * <p>Whether or not the wallet will ignore transactions that have a lockTime parameter > 0. By default, all such
     * transactions are ignored, because they are useful only in special protocols and such a transaction may not
     * confirm as fast as an app typically expects. By setting this property to true, you are acknowledging that
     * you understand what time-locked transactions are, and that your code is capable of handling them without risk.
     * For instance you are not providing anything valuable in return for an unconfirmed transaction that has a lock
     * time far in the future (which opens you up to Finney attacks).</p>
     *
     * <p>Note that this property is not serialized. So you have to set it to true each time you load or create a
     * wallet.</p>
     */
    public void setAcceptTimeLockedTransactions(boolean acceptTimeLockedTransactions) {
        this.acceptTimeLockedTransactions = acceptTimeLockedTransactions;
    }

    /**
     * See {@link Wallet#setAcceptTimeLockedTransactions(boolean)} for an explanation of this property.
     */
    public boolean doesAcceptTimeLockedTransactions() {
        return acceptTimeLockedTransactions;
    }

    /**
     * Uses protobuf serialization to save the wallet to the given file stream. To learn more about this file format, see
     * {@link WalletProtobufSerializer}.
     */
    public synchronized void saveToFileStream(OutputStream f) throws IOException {
        new MultiBitWalletProtobufSerializer().writeWallet(this, f);
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
            MultiBitWalletProtobufSerializer walletProtobufSerializer = new MultiBitWalletProtobufSerializer();
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
     * Called by the {@link BlockChain} when we receive a new filtered block that contains a transactions previously
     * received by a call to @{link receivePending}.<p>
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
    public synchronized void notifyTransactionIsInBlock(Sha256Hash txHash, StoredBlock block,
                                       BlockChain.NewBlockType blockType) throws VerificationException {
        Transaction tx = pending.get(txHash);
        if (tx == null)
            return;
        receive(tx, block, blockType, false);
    }

    /** The results of examining the dependency graph of a pending transaction for protocol abuse. */
    protected static class AnalysisResult {
        // Which tx, if any, had a non-zero lock time.
        Transaction timeLocked;
        // In future, depth, fees, if any are non-standard, anything else that's interesting ...
    }

    /**
     * <p>Called when we have found a transaction (via network broadcast or otherwise) that is relevant to this wallet
     * and want to record it. Note that we <b>cannot verify these transactions at all</b>, they may spend fictional
     * coins or be otherwise invalid. They are useful to inform the user about coins they can expect to receive soon,
     * and if you trust the sender of the transaction you can choose to assume they are in fact valid and will not
     * be double spent as an optimization.</p>
     *
     * <p>Before this method is called, {@link Wallet#isPendingTransactionRelevant(Transaction)} should have been
     * called to decide whether the wallet cares about the transaction - if it does, then this method expects the
     * transaction and any dependencies it has which are still in the memory pool.</p>
     */
    public synchronized void receivePending(Transaction tx, List<Transaction> dependencies) throws VerificationException {
        // Can run in a peer thread. This method will only be called if a prior call to isPendingTransactionRelevant
        // returned true, so we already know by this point that it sends coins to or from our wallet, or is a double
        // spend against one of our other pending transactions.
        //
        // Do a brief risk analysis of the transaction and its dependencies to check for any possible attacks.
        AnalysisResult analysis = analyzeTransactionAndDependencies(tx, dependencies);
        if (analysis.timeLocked != null && !doesAcceptTimeLockedTransactions()) {
            log.warn("Transaction {}, dependency of {} has a time lock value of {}", new Object[] {
                    analysis.timeLocked.getHashAsString(), tx.getHashAsString(), analysis.timeLocked.getLockTime()});
            return;
        }
        BigInteger valueSentToMe = tx.getValueSentToMe(this);
        BigInteger valueSentFromMe = tx.getValueSentFromMe(this);
        if (log.isInfoEnabled()) {
            log.info(String.format("Received a pending transaction %s that spends %s BTC from our own wallet," +
                    " and sends us %s BTC", tx.getHashAsString(), Utils.bitcoinValueToFriendlyString(valueSentFromMe),
                    Utils.bitcoinValueToFriendlyString(valueSentToMe)));
        }
        if (tx.getConfidence().getSource().equals(TransactionConfidence.Source.UNKNOWN)) {
            log.warn("Wallet received transaction with an unknown source. Consider tagging tx!");
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

    private AnalysisResult analyzeTransactionAndDependencies(Transaction tx, List<Transaction> dependencies) {
        AnalysisResult result = new AnalysisResult();
        if (tx.getLockTime() > 0)
            result.timeLocked = tx;
        if (dependencies != null) {
            for (Transaction dep : dependencies) {
                if (dep.getLockTime() > 0) {
                    result.timeLocked = dep;
                }
            }
        }
        return result;
    }

    /**
     * This method is used by a {@link Peer} to find out if a transaction that has been announced is interesting,
     * that is, whether we should bother downloading its dependencies and exploring the transaction to decide how
     * risky it is. If this method returns true then {@link Wallet#receivePending(Transaction, java.util.List)}
     * will soon be called with the transactions dependencies as well.
     */
    public boolean isPendingTransactionRelevant(Transaction tx) throws ScriptException {
        // Ignore it if we already know about this transaction. Receiving a pending transaction never moves it
        // between pools.
        EnumSet<Pool> containingPools = getContainingPools(tx);
        if (!containingPools.equals(EnumSet.noneOf(Pool.class))) {
            log.debug("Received tx we already saw in a block or created ourselves: " + tx.getHashAsString());
            return false;
        }

        // We only care about transactions that:
        //   - Send us coins
        //   - Spend our coins
        if (!isTransactionRelevant(tx)) {
            //log.debug("Received tx that isn't relevant to this wallet, discarding.");
            return false;
        }

        if (tx.getLockTime() > 0 && !acceptTimeLockedTransactions) {
            log.warn("Received transaction {} with a lock time of {}, but not configured to accept these, discarding",
                    tx.getHashAsString(), tx.getLockTime());
            return false;
        }
        
        log.debug("Saw relevant pending transaction " + tx.toString());

        return true;
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
     * <p>Returns true if the given transaction sends coins to any of our keys, or has inputs spending any of our outputs,
     * and if includeDoubleSpending is true, also returns true if tx has inputs that are spending outputs which are
     * not ours but which are spent by pending transactions.</p>
     *
     * <p>Note that if the tx has inputs containing one of our keys, but the connected transaction is not in the wallet,
     * it will not be considered relevant.</p>
     */
    @Override
    public synchronized boolean isTransactionRelevant(Transaction tx) throws ScriptException {
        return tx.isMine(this) || tx.getValueSentFromMe(this).compareTo(BigInteger.ZERO) > 0 ||
               tx.getValueSentToMe(this).compareTo(BigInteger.ZERO) > 0 ||
               findDoubleSpendAgainstPending(tx) != null;
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
        // due to a block replay so we do not want to do anything with it.
        // If it is on a sidechain then let the ELSE below deal with it
        // If it is a double spend it gets processed later.
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
     * <p>Updates the wallet by checking if this TX spends any of our outputs, and marking them as spent if so. It can
     * be called in two contexts. One is when we receive a transaction on the best chain but it wasn't pending, this
     * most commonly happens when we have a set of keys but the wallet transactions were wiped and we are catching up
     * with the block chain. It can also happen if a block includes a transaction we never saw at broadcast time.
     * If this tx double spends, it takes precedence over our pending transactions and the pending tx goes dead.</p>
     *
     * <p>The other context it can be called is from {@link Wallet#receivePending(Transaction, java.util.List)},
     * ie we saw a tx be broadcast or one was submitted directly that spends our own coins. If this tx double spends
     * it does NOT take precedence because the winner will be resolved by the miners - we assume that our version will
     * win, if we are wrong then when a block appears the tx will go dead.</p>
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
        if (tx == null) {
            throw new IllegalArgumentException("tx cannot be null");
        }
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

        /**
         * The AES key to use to decrypt the private key before signing.
         * If null then no decryption will be performed.
         */
        public KeyParameter aesKey = null;

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

        public boolean isCompleted() {
            return completed;
        }
    }

    /**
     * <p>Statelessly creates a transaction that sends the given value to address. The change is sent to
     * {@link Wallet#getChangeAddress()}, so you must have added at least one key.</p>
     *
     * <p>If you just want to send money quickly, you probably want
     * {@link Wallet#sendCoins(PeerGroup, Address, java.math.BigInteger)} instead. That will create the sending
     * transaction, commit to the wallet and broadcast it to the network all in one go. This method is lower level
     * and lets you see the proposed transaction before anything is done with it.</p>
     *
     * <p>This is a helper method that is equivalent to using {@link Wallet.SendRequest#to(Address, java.math.BigInteger)}
     * followed by {@link Wallet#completeTx(com.google.bitcoin.core.Wallet.SendRequest)} and returning the requests
     * transaction object. If you want more control over the process, just do those two steps yourself.</p>
     *
     * <p>IMPORTANT: This method does NOT update the wallet. If you call createSend again you may get two transactions
     * that spend the same coins. You have to call {@link Wallet#commitTx(Transaction)} on the created transaction to
     * prevent this, but that should only occur once the transaction has been accepted by the network. This implies
     * you cannot have more than one outstanding sending tx at once.</p>
     *
     * @param address       The BitCoin address to send the money to.
     * @param nanocoins     How much currency to send, in nanocoins.
     * @return either the created Transaction or null if there are insufficient coins.
     * coins as spent until commitTx is called on the result.
     */
    public synchronized Transaction createSend(Address address, BigInteger nanocoins) throws KeyCrypterException {
        SendRequest req = SendRequest.to(address, nanocoins);
        if (completeTx(req)) {
            return req.tx;
        } else {
            return null;  // No money.
        }
    }

    /**
     * Sends coins to the given address but does not broadcast the resulting pending transaction. It is still stored
     * in the wallet, so when the wallet is added to a {@link PeerGroup} or {@link Peer} the transaction will be
     * announced to the network. The given {@link SendRequest} is completed first using
     * {@link Wallet#completeTx(com.google.bitcoin.core.Wallet.SendRequest)} to make it valid.
     *
     * @return the Transaction that was created, or null if there are insufficient coins in the wallet.
     */
    public synchronized Transaction sendCoinsOffline(SendRequest request) throws KeyCrypterException {
        try {
            if (!completeTx(request))
                return null;  // Not enough money! :-(
            commitTx(request.tx);
            return request.tx;
        } catch (VerificationException e) {
            throw new RuntimeException(e);  // Cannot happen unless there's a bug, as we just created this ourselves.
        }
    }

    /**
     * <p>Sends coins to the given address, via the given {@link PeerGroup}. Change is returned to
     * {@link Wallet#getChangeAddress()}. No fee is attached <b>even if one would be required</b>.</p>
     *
     * <p>The returned object provides both the transaction, and a future that can be used to learn when the broadcast
     * is complete. Complete means, if the PeerGroup is limited to only one connection, when it was written out to
     * the socket. Otherwise when the transaction is written out and we heard it back from a different peer.</p>
     *
     * <p>Note that the sending transaction is committed to the wallet immediately, not when the transaction is
     * successfully broadcast. This means that even if the network hasn't heard about your transaction you won't be
     * able to spend those same coins again.</p>
     *
     * @param peerGroup a PeerGroup to use for broadcast or null.
     * @param to        Which address to send coins to.
     * @param value     How much value to send. You can use Utils.toNanoCoins() to calculate this.
     * @return An object containing the transaction that was created, and a future for the broadcast of it.
     */
    public SendResult sendCoins(PeerGroup peerGroup, Address to, BigInteger value) throws KeyCrypterException {
        SendRequest request = SendRequest.to(to, value);
        return sendCoins(peerGroup, request);
    }

    /**
     * <p>Sends coins according to the given request, via the given {@link PeerGroup}.</p>
     *
     * <p>The returned object provides both the transaction, and a future that can be used to learn when the broadcast
     * is complete. Complete means, if the PeerGroup is limited to only one connection, when it was written out to
     * the socket. Otherwise when the transaction is written out and we heard it back from a different peer.</p>
     *
     * <p>Note that the sending transaction is committed to the wallet immediately, not when the transaction is
     * successfully broadcast. This means that even if the network hasn't heard about your transaction you won't be
     * able to spend those same coins again.</p>
     *
     * @param peerGroup a PeerGroup to use for broadcast or null.
     * @param request the SendRequest that describes what to do, get one using static methods on SendRequest itself.
     * @return An object containing the transaction that was created, and a future for the broadcast of it.
     */
    public SendResult sendCoins(PeerGroup peerGroup, SendRequest request) throws KeyCrypterException {
        // Does not need to be synchronized as sendCoinsOffline is and the rest is all thread-local.

        // Commit the TX to the wallet immediately so the spent coins won't be reused.
        // TODO: We should probably allow the request to specify tx commit only after the network has accepted it.
        Transaction tx = sendCoinsOffline(request);
        if (tx == null)
            return null;  // Not enough money.
        SendResult result = new SendResult();
        result.tx = tx;
        // The tx has been committed to the pending pool by this point (via sendCoinsOffline -> commitTx), so it has
        // a txConfidenceListener registered. Once the tx is broadcast the peers will update the memory pool with the
        // count of seen peers, the memory pool will update the transaction confidence object, that will invoke the
        // txConfidenceListener which will in turn invoke the wallets event listener onTransactionConfidenceChanged
        // method.
        result.broadcastComplete = peerGroup.broadcastTransaction(tx);
        return result;
    }

    /**
     * Sends coins to the given address, via the given {@link Peer}. Change is returned to {@link Wallet#getChangeAddress()}.
     * If an exception is thrown by {@link Peer#sendMessage(Message)} the transaction is still committed, so the
     * pending transaction must be broadcast <b>by you</b> at some other time.
     *
     * @return The {@link Transaction} that was created or null if there was insufficient balance to send the coins.
     * @throws IOException if there was a problem broadcasting the transaction
     */
    public synchronized Transaction sendCoins(Peer peer, SendRequest request) throws IOException, KeyCrypterException {
        Transaction tx = sendCoinsOffline(request);
        if (tx == null)
            return null;  // Not enough money.
        peer.sendMessage(tx);
        return tx;
    }

    /**
     * Given a spend request containing an incomplete transaction, makes it valid by adding inputs and outputs according
     * to the instructions in the request. The transaction in the request is modified by this method.
     *
     * @param req a SendRequest that contains the incomplete transaction and details for how to make it valid.
     * @throws IllegalArgumentException if you try and complete the same SendRequest twice.
     * @throws KeyCrypterException if you try to sign an encrypted key without an AES key.
     * @return False if we cannot afford this send, true otherwise.
     */
//    public synchronized boolean completeTx(SendRequest req) throws IllegalStateException, KeyCrypterException {
//        // Calculate the transaction total
//        BigInteger nanocoins = BigInteger.ZERO;
//        for(TransactionOutput output : req.tx.getOutputs()) {
//            nanocoins = nanocoins.add(output.getValue());
//        }
//        final BigInteger total = nanocoins.add(req.fee);
//
//        log.info("Completing send tx with {} outputs totalling {}", req.tx.getOutputs().size(), bitcoinValueToFriendlyString(nanocoins));
//
//        // To send money to somebody else, we need to do gather up transactions with unspent outputs until we have
//        // sufficient value. Many coin selection algorithms are possible, we use a simple but suboptimal one.
//        // TODO: Sort coins so we use the smallest first, to combat wallet fragmentation and reduce fees.
//        BigInteger valueGathered = BigInteger.ZERO;
//        List<TransactionOutput> gathered = new LinkedList<TransactionOutput>();
//        for (Transaction tx : unspent.values()) {
//            // Do not try and spend coinbases that were mined too recently, the protocol forbids it.
//            if (!tx.isMature()) {
//                continue;
//            }
//            for (TransactionOutput output : tx.getOutputs()) {
//                if (!output.isAvailableForSpending()) continue;
//                if (!output.isMine(this)) continue;
//                gathered.add(output);
//                valueGathered = valueGathered.add(output.getValue());
//            }
//            if (valueGathered.compareTo(total) >= 0) break;
//        }
//        // Can we afford this?
//        if (valueGathered.compareTo(total) < 0) {
//            // If there are insufficient unspent coins, see if there are any pending coins that have change-back-to-self 
//            // and that have been seen by two or more peers. These are eligible for spending ("The Boomerang Rule")
//            for (Transaction tx : pending.values()) {
//                // Do not try and spend coinbases that were mined too recently, the protocol forbids it.
//                if (!tx.isMature()) {
//                    continue;
//                }
//                
//                if (transactionSpendsFromThisWalletAndHasBoomerangedBack(tx)) {
//                    for (TransactionOutput output : tx.getOutputs()) {
//                        if (!output.isAvailableForSpending())
//                            continue;
//                        if (!output.isMine(this))
//                            continue;
//
//                        gathered.add(output);
//                        valueGathered = valueGathered.add(output.getValue());
//                    }
//                    if (valueGathered.compareTo(total) >= 0)
//                        break;
//                }
//            }   
//            
//            if (valueGathered.compareTo(total) < 0) {
//                // Still not enough funds.
//                log.info("Insufficient value in wallet for send, missing "
//                        + bitcoinValueToFriendlyString(nanocoins.subtract(valueGathered)));
//                // TODO: Should throw an exception here.
//                return false;
//            }
//        }
//        checkState(gathered.size() > 0);
//        req.tx.getConfidence().setConfidenceType(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
//        BigInteger change = valueGathered.subtract(total);
//        if (change.compareTo(BigInteger.ZERO) > 0) {
//            // The value of the inputs is greater than what we want to send. Just like in real life then,
//            // we need to take back some coins ... this is called "change". Add another output that sends the change
//            // back to us. The address comes either from the request or getChangeAddress() as a default.
//            Address changeAddress = req.changeAddress != null ? req.changeAddress : getChangeAddress();
//            log.info("  with {} coins change", bitcoinValueToFriendlyString(change));
//            req.tx.addOutput(new TransactionOutput(params, req.tx, change, changeAddress));
//        }
//        for (TransactionOutput output : gathered) {
//            req.tx.addInput(output);
//        }
//
//        // Now sign the inputs, thus proving that we are entitled to redeem the connected outputs.
//        try {
//            req.tx.signInputs(Transaction.SigHash.ALL, this, req.aesKey);
//        } catch (ScriptException e) {
//            // If this happens it means an output script in a wallet tx could not be understood. That should never
//            // happen, if it does it means the wallet has got into an inconsistent state.
//            throw new RuntimeException(e);
//        } catch (KeyCrypterException kce) {
//            // This can occur if the key being used is encrypted.
//            throw new RuntimeException(kce);
//        }
//
//        // Keep a track of the date the tx was created (used in MultiBitService
//        // to work out the block it appears in).
//        req.tx.setUpdateTime(new Date());
//        req.completed = true;
//        log.info("  completed {}", req.tx.getHashAsString());
//        return true;
//    }

    public synchronized boolean completeTx(SendRequest req) throws KeyCrypterException {
        Preconditions.checkArgument(!req.completed, "Given SendRequest has already been completed.");
        // Calculate the amount of value we need to import.
        BigInteger value = BigInteger.ZERO;
        for (TransactionOutput output : req.tx.getOutputs()) {
            value = value.add(output.getValue());
        }
        value = value.add(req.fee);

        log.info("Completing send tx with {} outputs totalling {}",
                req.tx.getOutputs().size(), bitcoinValueToFriendlyString(value));

        // Calculate a list of ALL potential candidates for spending and then ask a coin selector to provide us
        // with the actual outputs that'll be used to gather the required amount of value. In this way, users
        // can customize coin selection policies.
        //
        // Note that this code is poorly optimized: the spend candidates only alter when transactions in the wallet
        // change - it could be pre-calculated and held in RAM, and this is probably an optimization worth doing.
        // Note that output.isMine(this) needs to test the keychain which is currently an array, so it's
        // O(candidate outputs ^ keychain.size())! There's lots of low hanging fruit here.
        LinkedList<TransactionOutput> candidates = calculateSpendCandidates(true);
        // Of the coins we could spend, pick some that we actually will spend.
        CoinSelection selection = coinSelector.select(value, candidates);
        // Can we afford this?
        if (selection.valueGathered.compareTo(value) < 0) {
            log.warn("Insufficient value in wallet for send, missing " +
                    bitcoinValueToFriendlyString(value.subtract(selection.valueGathered)));
            // TODO: Should throw an exception here.
            return false;
        }
        checkState(selection.gathered.size() > 0);
        req.tx.getConfidence().setConfidenceType(TransactionConfidence.ConfidenceType.NOT_SEEN_IN_CHAIN);
        BigInteger change = selection.valueGathered.subtract(value);
        if (change.compareTo(BigInteger.ZERO) > 0) {
            // The value of the inputs is greater than what we want to send. Just like in real life then,
            // we need to take back some coins ... this is called "change". Add another output that sends the change
            // back to us. The address comes either from the request or getChangeAddress() as a default.
            Address changeAddress = req.changeAddress != null ? req.changeAddress : getChangeAddress();
            log.info("  with {} coins change", bitcoinValueToFriendlyString(change));
            req.tx.addOutput(new TransactionOutput(params, req.tx, change, changeAddress));
        }
        for (TransactionOutput output : selection.gathered) {
            req.tx.addInput(output);
        }

        // Now sign the inputs, thus proving that we are entitled to redeem the connected outputs.
        try {
            req.tx.signInputs(Transaction.SigHash.ALL, this, req.aesKey);
        } catch (ScriptException e) {
            // If this happens it means an output script in a wallet tx could not be understood. That should never
            // happen, if it does it means the wallet has got into an inconsistent state.
            throw new RuntimeException(e);
        }

        // Label the transaction as being self created. We can use this later to spend its change output even before
        // the transaction is confirmed.
        req.tx.getConfidence().setSource(TransactionConfidence.Source.SELF);

      // Keep a track of the date the tx was created (used in MultiBitService
      // to work out the block it appears in).
      req.tx.setUpdateTime(new Date());
        req.completed = true;
        log.info("  completed {}", req.tx.getHashAsString());
        return true;
    }

    private LinkedList<TransactionOutput> calculateSpendCandidates(boolean excludeImmatureCoinbases) {
        LinkedList<TransactionOutput> candidates = Lists.newLinkedList();
        for (Transaction tx : Iterables.concat(unspent.values(), pending.values())) {
            // Do not try and spend coinbases that were mined too recently, the protocol forbids it.
            if (excludeImmatureCoinbases && !tx.isMature()) continue;
            for (TransactionOutput output : tx.getOutputs()) {
                if (!output.isAvailableForSpending()) continue;
                if (!output.isMine(this)) continue;
                candidates.add(output);
            }
        }
        return candidates;
    }

//    private boolean transactionSpendsFromThisWalletAndHasBoomerangedBack(Transaction tx) {
//        TransactionConfidence confidence = tx.getConfidence();
//        if (confidence == null || confidence.getBroadcastByCount() < MINIMUM_NUMBER_OF_PEERS_A_TRANSACTION_IS_SEEN_BY_FOR_SPEND) {
//            // Transaction has not been seen by enough peers.
//            return false;
//        }
//         
//        boolean wasSentFromMyWallet = true;
//        // Check all transaction inputs are from your own wallet
//        for (TransactionInput input : tx.getInputs()) {
//            TransactionOutPoint outPoint = input.getOutpoint();
//            try {
//                TransactionOutput transactionOutput = outPoint.getConnectedOutput();
//                if (transactionOutput == null) {
//                    wasSentFromMyWallet = false;
//                    break;
//                }
//                ECKey ecKey = outPoint.getConnectedKey(this);
//                // if no ecKey was found then the transaction uses a transaction output from somewhere else
//                if (ecKey == null) {
//                    wasSentFromMyWallet = false;
//                    break;
//                }
//            } catch (ScriptException e) {
//                wasSentFromMyWallet = false;
//                break;
//            }
//        }
//        
//        if (!wasSentFromMyWallet) return false;
//        
//        // Transaction is both from out wallet and has boomeranged back.
//        return true;
//    }

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
     * @throws KeyCrypterException
     */
    public synchronized int addKeys(final List<ECKey> keys) throws KeyCrypterException {
        // TODO: Consider making keys a sorted list or hashset so membership testing is faster.
        int added = 0;
        for (final ECKey key : keys) {
            if (keychain.contains(key)) continue;

            // If the key has a keyCrypter that does not match the Wallet's then a KeyCrypterException is thrown.
            // This is done because only one keyCrypter is persisted per Wallet and hence all the keys must be homogenous.
            if (keyCrypter != null && keyCrypter.getEncryptionType() != EncryptionType.UNENCRYPTED &&
                    !keyCrypter.equals(key.getKeyCrypter())) {
                throw new KeyCrypterException("Cannot add key " + key.toString() + " because the keyCrypter does not match the wallets. Keys must be homogenous.");
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
     * <p>It's possible to calculate a wallets balance from multiple points of view. This enum selects which
     * getBalance() should use.</p>
     *
     * <p>Consider a real-world example: you buy a snack costing $5 but you only have a $10 bill. At the start you have
     * $10 viewed from every possible angle. After you order the snack you hand over your $10 bill. From the
     * perspective of your wallet you have zero dollars (AVAILABLE). But you know in a few seconds the shopkeeper
     * will give you back $5 change so most people in practice would say they have $5 (ESTIMATED).</p>
     */
    public enum BalanceType {
        /**
         * Balance calculated assuming all pending transactions are in fact included into the best chain by miners.
         * This includes the value of immature coinbase transactions.
         */
        ESTIMATED,

        /**
         * Balance that can be safely used to create new spends. This is whatever the default coin selector would
         * make available, which by default means transaction outputs with at least 1 confirmation and pending
         * transactions created by our own wallet which have been propagated across the network.
         */
        AVAILABLE
    }

    /**
     * Returns the AVAILABLE balance of this wallet. See {@link BalanceType#AVAILABLE} for details on what this
     * means.
     */
    public synchronized BigInteger getBalance() {
        return getBalance(BalanceType.AVAILABLE);
    }

    /**
     * Returns the balance of this wallet as calculated by the provided balanceType.
     */
    public synchronized BigInteger getBalance(BalanceType balanceType) {
        if (balanceType == BalanceType.AVAILABLE) {
            return getBalance(coinSelector);
        } else if (balanceType == BalanceType.ESTIMATED) {
            LinkedList<TransactionOutput> all = calculateSpendCandidates(false);
            BigInteger value = BigInteger.ZERO;
            for (TransactionOutput out : all) value = value.add(out.getValue());
            return value;
        } else {
            throw new AssertionError("Unknown balance type");  // Unreachable.
        }
    }

    /**
     * Returns the balance that would be considered spendable by the given coin selector. Just asks it to select
     * as many coins as possible and returns the total.
     */
    public synchronized BigInteger getBalance(CoinSelector selector) {
        checkNotNull(selector);
        LinkedList<TransactionOutput> candidates = calculateSpendCandidates(true);
        CoinSelection selection = selector.select(params.MAX_MONEY, candidates);
        return selection.valueGathered;
    }

    @Override
    public synchronized String toString() {
        return toString(false, null);
    }

    /**
     * Formats the wallet as a human readable piece of text. Intended for debugging, the format is not meant to be
     * stable or human readable.
     * @param includePrivateKeys Whether raw private key data should be included.
     * @param chain If set, will be used to estimate lock times for block timelocked transactions.
     * @return
     */
    public synchronized String toString(boolean includePrivateKeys, AbstractBlockChain chain) {
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
            toStringHelper(builder, unspent, chain);
        }
        if (spent.size() > 0) {
            builder.append("\nSPENT:\n");
            toStringHelper(builder, spent, chain);
        }
        if (pending.size() > 0) {
            builder.append("\nPENDING:\n");
            toStringHelper(builder, pending, chain);
        }
        if (inactive.size() > 0) {
            builder.append("\nINACTIVE:\n");
            toStringHelper(builder, inactive, chain);
        }
        if (dead.size() > 0) {
            builder.append("\nDEAD:\n");
            toStringHelper(builder, dead, chain);
        }

        // Add the keyCrypter so that any setup parameters are in the wallet toString.
        if (this.keyCrypter != null) {
            builder.append("\n keyCrypter: " + keyCrypter.toString());
        }
        return builder.toString();
    }

    private void toStringHelper(StringBuilder builder, Map<Sha256Hash, Transaction> transactionMap,
                                AbstractBlockChain chain) {
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
            builder.append(tx.toString(chain));
        }
    }

    /**
     * <p>Don't call this directly. It's not intended for API users.</p>
     *
     * <p>Called by the {@link BlockChain} when the best chain (representing total work done) has changed. In this case,
     * we need to go through our transactions and find out if any have become invalid. It's possible for our balance
     * to go down in this case: money we thought we had can suddenly vanish if the rest of the network agrees it
     * should be so.</p>
     *
     * <p>The oldBlocks/newBlocks lists are ordered height-wise from top first to bottom last.</p>
     */
    public synchronized void reorganize(StoredBlock splitPoint, List<StoredBlock> oldBlocks,
                                        List<StoredBlock> newBlocks) throws VerificationException {
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
        Set<Transaction> connectedTransactions = new HashSet<Transaction>();
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
                    //log.debug("Wallet#removeEntriesAfterDate - Removed tx.1 " + member.getValue());
                    continue;
                }
                
                // if no updateTime remove them
                if (updateTime == null || updateTime.getTime() == 0) {
                    iterator.remove();
                    //log.debug("Removed tx.2 " + member.getValue());
                    continue;                    
                }
            }
        }
    }

    /**
     * Encrypt the wallet using the KeyCrypter and the AES key.
     *
     * @param keyCrypter The KeyCrypter that specifies how to encrypt/ decrypt a key
     * @param aesKey AES key to use (normally created using KeyCrypter#deriveKey and cached as it is time consuming to create from a password)
     * @throws KeyCrypterException Thrown if the wallet encryption fails. If so, the wallet state is unchanged.
     */
    synchronized public void encrypt(KeyCrypter keyCrypter, KeyParameter aesKey) throws KeyCrypterException {
        if (keyCrypter == null) {
            throw new KeyCrypterException("A keyCrypter must be specified to encrypt a wallet.");
        }

        /**
         * If the wallet is already encrypted then you cannot encrypt it again.
         */
        if (getEncryptionType() != EncryptionType.UNENCRYPTED) {
            throw new WalletIsAlreadyEncryptedException("Wallet is already encrypted");
        }

        // Create a new arraylist that will contain the encrypted keys
        ArrayList<ECKey> encryptedKeyChain = new ArrayList<ECKey>();

        for (ECKey key : keychain) {
            if (key.isEncrypted()) {
                throw new WalletIsAlreadyEncryptedException("Key '" + key.toString() + "' is already encrypted.");
            }

            // Encrypt the key.
            ECKey encryptedKey = key.encrypt(keyCrypter, aesKey);

            // Check that the encrypted key can be successfully decrypted.
            if (!ECKey.encryptionIsReversible(key, encryptedKey, keyCrypter, aesKey)) {
                // Abort encryption
                throw new KeyCrypterException("The key " + key.toString() + " cannot be successfully decrypted after encryption so aborting wallet encryption.");
            }

            encryptedKeyChain.add(encryptedKey);
        }

        // Replace the old keychain with the encrypted one.
        keychain = encryptedKeyChain;

        // The wallet is now encrypted.
        this.keyCrypter = keyCrypter;
    }

    /**
     * Decrypt the wallet with the wallets keyCrypter and AES key.
     *
     * @param aesKey AES key to use (normally created using KeyCrypter#deriveKey and cached as it is time consuming to create from a password)
     * @throws KeyCrypterException Thrown if the wallet decryption fails. If so, the wallet state is unchanged.
     */
    synchronized public void decrypt(KeyParameter aesKey) throws KeyCrypterException {
        // Check the wallet is already encrypted - you cannot decrypt an unencrypted wallet.
        if (getEncryptionType() == EncryptionType.UNENCRYPTED) {
            throw new WalletIsAlreadyDecryptedException("Wallet is already decrypted");
        }

        // Check that the wallet keyCrypter is non-null.
        // This is set either at construction (if an encrypted wallet is created) or by wallet encryption.
        if (keyCrypter == null) {
            throw new KeyCrypterException("The wallet keyCrypter is null so cannot decrypt.");
        }

        // Create a new arraylist that will contain the decrypted keys
        ArrayList<ECKey> decryptedKeyChain = new ArrayList<ECKey>();

        for (ECKey key : keychain) {
            if (!key.isEncrypted()) {
                throw new WalletIsAlreadyDecryptedException("Key '" + key.toString() + "' is already decrypted.");
            }

            // Decrypt the key.
            ECKey decryptedECKey = key.decrypt(keyCrypter, aesKey);
            decryptedKeyChain.add(decryptedECKey);
        }

        // Replace the old keychain with the unencrypted one.
        keychain = decryptedKeyChain;

        // The wallet is now unencrypted.
        keyCrypter = null;
    }

    /**
     *  Check whether the password can decrypt the first key in the wallet.
     *  This can be used to check the validity of an entered password.
     *
     *  @throws KeyCrypterException An exception is thrown if the AES key could not be derived from the password.
     *  @returns boolean true if password supplied can decrypt the first private key in the wallet, false otherwise.
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
     *
     *  @returns boolean true if AES key supplied can decrypt the first private key in the wallet, false otherwise.
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

    /**
     * Get the wallet's KeyCrypter.
     * (Used in encrypting/ decrypting an ECKey).
     */
    public KeyCrypter getKeyCrypter() {
        return keyCrypter;
    }

    /**
     * Get the type of encryption used for this wallet.
     *
     * (This is a convenience method - the encryption type is actually stored in the keyCrypter).
     */
    public EncryptionType getEncryptionType() {
        if (keyCrypter == null) {
            // Unencrypted wallet.
            return EncryptionType.UNENCRYPTED;
        } else {
            return keyCrypter.getEncryptionType();
        }
    }

    public MultiBitWalletVersion getVersion() {
        return version;
    }

    public void setVersion(MultiBitWalletVersion version) {
        this.version = version;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    /**
     * Gets the number of elements that will be added to a bloom filter returned by getBloomFilter
     */
    public int getBloomFilterElementCount() {
        int size = getKeychainSize() * 2;
        for (Transaction tx : getTransactions(false, true)) {
            for (TransactionOutput out : tx.getOutputs()) {
                try {
                    if (out.isMine(this) && out.getScriptPubKey().isSentToRawPubKey())
                        size++;
                } catch (ScriptException e) {
                    throw new RuntimeException(e); // If it is ours, we parsed the script corectly, so this shouldn't happen
                }
            }
        }
        return size;
    }
    
    /**
     * Gets a bloom filter that contains all of the public keys from this wallet,
     * and which will provide the given false-positive rate.
     * 
     * See the docs for {@link BloomFilter#BloomFilter(int, double)} for a brief explanation of anonymity when using bloom filters.
     */
    public BloomFilter getBloomFilter(double falsePositiveRate) {
        return getBloomFilter(getBloomFilterElementCount(), falsePositiveRate, new Random().nextLong());
    }
    
    /**
     * Gets a bloom filter that contains all of the public keys from this wallet,
     * and which will provide the given false-positive rate if it has size elements.
     * Keep in mind that you will get 2 elements in the bloom filter for each key in the wallet.
     * 
     * This is used to generate a BloomFilter which can be #{link BloomFilter.merge}d with another.
     * It could also be used if you have a specific target for the filter's size.
     * 
     * See the docs for {@link BloomFilter#BloomFilter(int, double)} for a brief explanation of anonymity when using bloom filters.
     */
    public synchronized BloomFilter getBloomFilter(int size, double falsePositiveRate, long nTweak) {
        BloomFilter filter = new BloomFilter(size, falsePositiveRate, nTweak);
        for (ECKey key : keychain) {
            filter.insert(key.getPubKey());
            filter.insert(key.getPubKeyHash());
        }
        for (Transaction tx : getTransactions(false, true)) {
            for (int i = 0; i < tx.getOutputs().size(); i++) {
                TransactionOutput out = tx.getOutputs().get(i);
                try {
                    if (out.isMine(this) && out.getScriptPubKey().isSentToRawPubKey()) {
                        TransactionOutPoint outPoint = new TransactionOutPoint(params, i, tx);
                        filter.insert(outPoint.bitcoinSerialize());
                    }
                } catch (ScriptException e) {
                    throw new RuntimeException(e); // If it is ours, we parsed the script corectly, so this shouldn't happen
                }
            }
        }
        return filter;
    }

    /** Returns the {@link CoinSelector} object which controls which outputs can be spent by this wallet. */
    public synchronized CoinSelector getCoinSelector() {
        return coinSelector;
    }

    /**
     * A coin selector is responsible for choosing which outputs to spend when creating transactions. The default
     * selector implements a policy of spending transactions that appeared in the best chain and pending transactions
     * that were created by this wallet, but not others.
     */
    public synchronized void setCoinSelector(CoinSelector coinSelector) {
        this.coinSelector = coinSelector;
    }
}
