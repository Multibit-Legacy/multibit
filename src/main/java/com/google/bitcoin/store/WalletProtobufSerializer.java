/**
 * Copyright 2012 Google Inc.
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

package com.google.bitcoin.store;

import com.google.bitcoin.core.*;
import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;
import com.google.common.base.Preconditions;
import com.google.protobuf.ByteString;
import com.google.protobuf.TextFormat;
import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.WrappedWallet;
import org.multibit.IsMultiBitClass;
import org.multibit.crypto.EncryptedPrivateKey;
import org.multibit.crypto.EncrypterDecrypter;
import org.multibit.crypto.EncrypterDecrypterScrypt;
import org.multibit.crypto.ScryptParameters;
import org.multibit.file.WalletVersionException;
import org.multibit.model.WalletMajorVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Serialize and de-serialize a wallet to a byte stream containing a
 * <a href="http://code.google.com/apis/protocolbuffers/docs/overview.html">protocol buffer</a>. Protocol buffers are
 * a data interchange format developed by Google with an efficient binary representation, a type safe specification
 * language and compilers that generate code to work with those data structures for many languages. Protocol buffers
 * can have their format evolved over time: conceptually they represent data using (tag, length, value) tuples. The
 * format is defined by the <tt>bitcoin.proto</tt> file in the BitCoinJ source distribution.<p>
 *
 * This class is used through its static methods. The most common operations are writeWallet and readWallet, which do
 * the obvious operations on Output/InputStreams. You can use a {@link java.io.ByteArrayInputStream} and equivalent
 * {@link java.io.ByteArrayOutputStream} if you'd like byte arrays instead. The protocol buffer can also be manipulated
 * in its object form if you'd like to modify the flattened data structure before serialization to binary.<p>
 *
 * You can extend the wallet format with additional fields specific to your application if you want, but make sure
 * to either put the extra data in the provided extension areas, or select tag numbers that are unlikely to be used
 * by anyone else.<p>
 * 
 * @author Miron Cuperman
 */
public class WalletProtobufSerializer implements IsMultiBitClass {
    private static final Logger log = LoggerFactory.getLogger(WalletProtobufSerializer.class);

    // Used for de-serialization
    private Map<ByteString, Transaction> txMap;
    
    private WalletProtobufSerializer() {
        txMap = new HashMap<ByteString, Transaction>();
    }

    /**
     * Formats the given WrappedWallet to the given output stream in protocol buffer format.<p>
     */
    public static void writeWrappedWallet(org.multibit.model.WrappedWallet wrappedWallet, OutputStream output) throws IOException {
        Protos.WrappedWallet wrappedWalletProto = wrappedWalletToProto(wrappedWallet);
        wrappedWalletProto.writeTo(output);
    }

    /**
     * Formats the given wallet (transactions and keys) to the given output stream in protocol buffer format.<p>
     *     
     * Equivalent to <tt>walletToProto(wallet).writeTo(output);</tt>
     */
    public static void writeWallet(Wallet wallet, OutputStream output) throws IOException {
        Protos.Wallet walletProto = walletToProto(wallet);
        walletProto.writeTo(output);
    }

    /**
     * Returns the given wallet formatted as text. The text format is that used by protocol buffers and although it
     * can also be parsed using {@link TextFormat#merge(CharSequence, com.google.protobuf.Message.Builder)},
     * it is designed more for debugging than storage. It is not well specified and wallets are largely binary data
     * structures anyway, consisting as they do of keys (large random numbers) and {@link Transaction}s which also
     * mostly contain keys and hashes.
     */
    public static String walletToText(Wallet wallet) {
        Protos.Wallet walletProto = walletToProto(wallet);
        return TextFormat.printToString(walletProto);
    }

    /**
     * Converts the given wrappedWallet to the object representation of the protocol buffers. This can be modified, or
     * additional data fields set, before serialization takes place.
     */
    public static Protos.WrappedWallet wrappedWalletToProto(org.multibit.model.WrappedWallet wrappedWallet) {
        Protos.WrappedWallet.Builder wrappedWalletBuilder = Protos.WrappedWallet.newBuilder();
        Protos.Wallet.Builder protoWalletBuilder = walletToProtoBuilder(wrappedWallet.getWallet());
        wrappedWalletBuilder.setWallet(protoWalletBuilder);
        
        return wrappedWalletBuilder.build();
    }
    
    /**
     * Converts the given wallet to the object representation of the protocol buffers. This can be modified, or
     * additional data fields set, before serialization takes place.
     */
    public static Protos.Wallet walletToProto(Wallet wallet) {
        return walletToProtoBuilder(wallet).build();
    }
    
    private static Protos.Wallet.Builder walletToProtoBuilder(Wallet wallet) {
        Protos.Wallet.Builder walletBuilder = Protos.Wallet.newBuilder();
        walletBuilder.setNetworkIdentifier(wallet.getNetworkParameters().getId());
        for (WalletTransaction wtx : wallet.getWalletTransactions()) {
            Protos.Transaction txProto = makeTxProto(wtx);
            walletBuilder.addTransaction(txProto);
        }
        
        for (ECKey key : wallet.getKeys()) {
            Protos.Key.Builder buf = Protos.Key.newBuilder().setCreationTimestamp(key.getCreationTimeSeconds() * 1000)
                                                         // .setLabel() TODO
                                                            .setType(Protos.Key.Type.ORIGINAL);
            if (key.getPrivKeyBytes() != null) {
                buf.setPrivateKey(ByteString.copyFrom(key.getPrivKeyBytes()));
            }

            
            EncryptedPrivateKey encryptedPrivateKey = key.getEncryptedPrivateKey();
            if (encryptedPrivateKey != null) {
                Protos.EncryptedPrivateKey.Builder encryptedKeyBuilder = Protos.EncryptedPrivateKey.newBuilder()
                    .setEncryptedPrivateKey(ByteString.copyFrom(encryptedPrivateKey.getEncryptedBytes()))
                    .setInitialisationVector(ByteString.copyFrom(encryptedPrivateKey.getInitialisationVector()));
                buf.setEncryptedPrivateKey(encryptedKeyBuilder);
            }

            // We serialize the public key even if the private key is present for speed reasons: we don't want to do
            // lots of slow EC math to load the wallet, we prefer to store the redundant data instead. It matters more
            // on mobile platforms.
            buf.setPublicKey(ByteString.copyFrom(key.getPubKey()));
            walletBuilder.addKey(buf);
        }

        // Populate the lastSeenBlockHash field.
        Sha256Hash lastSeenBlockHash = wallet.getLastBlockSeenHash();
        if (lastSeenBlockHash != null) {
            walletBuilder.setLastSeenBlockHash(hashToByteString(lastSeenBlockHash));
        }
        
        // Populate the scrypt parameters.
        EncrypterDecrypter encrypterDecrypter = wallet.getEncrypterDecrypter();
        if (encrypterDecrypter instanceof EncrypterDecrypterScrypt) {
            EncrypterDecrypterScrypt encrypterDecrypterScrypt = (EncrypterDecrypterScrypt)encrypterDecrypter;
            ScryptParameters scryptParameters = encrypterDecrypterScrypt.getScryptParameters();
            Protos.ScryptParameters.Builder scryptParametersBuilder = Protos.ScryptParameters.newBuilder()
                .setSalt(ByteString.copyFrom(scryptParameters.getSalt()))
                .setN(scryptParameters.getN())
                .setP(scryptParameters.getP())
                .setR(scryptParameters.getR());
            walletBuilder.setEncryptionParameters(scryptParametersBuilder);
        }
        
        // Populate the wallet type.
        if (wallet.getEncryptionType() == EncryptionType.ENCRYPTED_SCRYPT_AES) {
            walletBuilder.setEncryptionType(Protos.Wallet.EncryptionType.ENCRYPTED_SCRYPT_AES);
        } else {
            walletBuilder.setEncryptionType(Protos.Wallet.EncryptionType.UNENCRYPTED);
        }
        
        // Populate the major and minor wallet version.
        if (wallet.getMajorVersion() != null) {
            walletBuilder.setMajorVersion(wallet.getMajorVersion().getWalletVersionAsInt());
        }
        walletBuilder.setMinorVersion(wallet.getMinorVersion());
        

        return walletBuilder;
    }
    
    private static Protos.Transaction makeTxProto(WalletTransaction wtx) {
        Transaction tx = wtx.getTransaction();
        Protos.Transaction.Builder txBuilder = Protos.Transaction.newBuilder();
        
        txBuilder.setPool(Protos.Transaction.Pool.valueOf(wtx.getPool().getValue()))
                 .setHash(hashToByteString(tx.getHash()))
                 .setVersion((int) tx.getVersion());

        if (tx.getUpdateTime() != null) {
            txBuilder.setUpdatedAt(tx.getUpdateTime().getTime());
        }
        
        if (tx.getLockTime() > 0) {
            txBuilder.setLockTime((int)tx.getLockTime());
        }
        
        // Handle inputs.
        for (TransactionInput input : tx.getInputs()) {
            Protos.TransactionInput.Builder inputBuilder = Protos.TransactionInput.newBuilder()
                .setScriptBytes(ByteString.copyFrom(input.getScriptBytes()))
                .setTransactionOutPointHash(hashToByteString(input.getOutpoint().getHash()))
                .setTransactionOutPointIndex((int) input.getOutpoint().getIndex());
            if (input.hasSequence()) {
                inputBuilder.setSequence((int)input.getSequence());
            }
            txBuilder.addTransactionInput(inputBuilder);
        }
        
        // Handle outputs.
        for (TransactionOutput output : tx.getOutputs()) {
            Protos.TransactionOutput.Builder outputBuilder = Protos.TransactionOutput.newBuilder()
                .setScriptBytes(ByteString.copyFrom(output.getScriptBytes()))
                .setValue(output.getValue().longValue());
            final TransactionInput spentBy = output.getSpentBy();
            if (spentBy != null) {
                Sha256Hash spendingHash = spentBy.getParentTransaction().getHash();
                int spentByTransactionIndex = spentBy.getParentTransaction().getInputs().indexOf(spentBy);
                outputBuilder.setSpentByTransactionHash(hashToByteString(spendingHash))
                             .setSpentByTransactionIndex(spentByTransactionIndex);
            }
            txBuilder.addTransactionOutput(outputBuilder);
        }
        
        // Handle which blocks tx was seen in.
        if (tx.getAppearsInHashes() != null) {
            for (Sha256Hash hash : tx.getAppearsInHashes()) {
                txBuilder.addBlockHash(hashToByteString(hash));
            }
        }
        
        if (tx.hasConfidence()) {
            TransactionConfidence confidence = tx.getConfidence();
            Protos.TransactionConfidence.Builder confidenceBuilder = Protos.TransactionConfidence.newBuilder();
            writeConfidence(txBuilder, confidence, confidenceBuilder);
        }
        
        return txBuilder.build();
    }

    private static void writeConfidence(Protos.Transaction.Builder txBuilder,
                                        TransactionConfidence confidence,
                                        Protos.TransactionConfidence.Builder confidenceBuilder) {
        confidenceBuilder.setType(Protos.TransactionConfidence.Type.valueOf(confidence.getConfidenceType().getValue()));
        if (confidence.getConfidenceType() == ConfidenceType.BUILDING) {
            confidenceBuilder.setAppearedAtHeight(confidence.getAppearedAtChainHeight());
        }
        if (confidence.getConfidenceType() == ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND) {
            Sha256Hash overridingHash = confidence.getOverridingTransaction().getHash();
            confidenceBuilder.setOverridingTransaction(hashToByteString(overridingHash));
        }
        txBuilder.setConfidence(confidenceBuilder);
    }

    private static ByteString hashToByteString(Sha256Hash hash) {
        return ByteString.copyFrom(hash.getBytes());
    }

    private static Sha256Hash byteStringToHash(ByteString bs) {
        return new Sha256Hash(bs.toByteArray());
    }

    /**
     * Parses a VersionableWallet from the given stream. The stream is expected to contain a binary serialization of a 
     * {@link Protos.WrappedWallet} object.<p>
     *  
     * If the stream is invalid or the serialized wallet contains unsupported features, 
     * {@link IllegalArgumentException} is thrown.
     *
     */
    public static org.multibit.model.WrappedWallet readWrappedWallet(InputStream input) throws IOException {
        Protos.WrappedWallet wrappedWalletProto = Protos.WrappedWallet.parseFrom(input);

        // System.out.println(TextFormat.printToString(versionableWalletProto));
        Wallet wallet = parseWalletProto(wrappedWalletProto.getWallet());

        org.multibit.model.WrappedWallet wrappedWallet = new org.multibit.model.WrappedWallet(wallet);
        
        return wrappedWallet;
    }

    /**
     * Parses a wallet from the given stream. The stream is expected to contain a binary serialization of a 
     * {@link Protos.Wallet} object.<p>
     *     
     * If the stream is invalid or the serialized wallet contains unsupported features, 
     * {@link IllegalArgumentException} is thrown.
     *
     */
    public static Wallet readWallet(InputStream input) throws IOException {
        Protos.Wallet walletProto = Protos.Wallet.parseFrom(input);

        // System.out.println(TextFormat.printToString(walletProto));
        Wallet wallet = parseWalletProto(walletProto);

        return wallet;
    }
    
    private static Wallet parseWalletProto(Protos.Wallet walletProto) {
        // TODO: This method should throw more specific exception types than IllegalArgumentException.
        WalletProtobufSerializer serializer = new WalletProtobufSerializer();

        NetworkParameters params = NetworkParameters.fromID(walletProto.getNetworkIdentifier());
        
        // Read the scrypt parameters.
        Protos.ScryptParameters encryptionParameters = walletProto.getEncryptionParameters();
        ScryptParameters scryptParameters = new ScryptParameters(encryptionParameters.getSalt().toByteArray(), (int)encryptionParameters.getN(), encryptionParameters.getR(), encryptionParameters.getP());
        EncrypterDecrypter encrypterDecrypter = new EncrypterDecrypterScrypt(scryptParameters);
      
        Wallet wallet = new Wallet(params, encrypterDecrypter);

        // Read the WalletType.
        EncryptionType walletType = null;
        if (walletProto.hasEncryptionType()) {
            // There is a wallet type specified so use it directly.
            Protos.Wallet.EncryptionType protoWalletType = walletProto.getEncryptionType();
            if (protoWalletType.getNumber() == Protos.Wallet.EncryptionType.ENCRYPTED_SCRYPT_AES_VALUE) {
                walletType = EncryptionType.ENCRYPTED_SCRYPT_AES;
            } else  if (protoWalletType.getNumber() == Protos.Wallet.EncryptionType.UNENCRYPTED_VALUE) {
                walletType = EncryptionType.UNENCRYPTED;
            } else {
                // Unknown wallet type - something from the future.
                throw new IllegalArgumentException("Unknown wallet type of '" + protoWalletType.toString());
            }
        } else {
            // If not specified, grandfather in as unencrypted.
            walletType = EncryptionType.UNENCRYPTED;
        }
        wallet.setEncryptionType(walletType);

        boolean isWalletCurrentlyEncrypted = false;
        // Read all keys
        for (Protos.Key keyProto : walletProto.getKeyList()) {
            if (keyProto.getType() != Protos.Key.Type.ORIGINAL) {
                throw new IllegalArgumentException("Unknown key type in wallet");
            }

            byte[] privKey = keyProto.hasPrivateKey() ? keyProto.getPrivateKey().toByteArray() : null;
            EncryptedPrivateKey encryptedPrivateKey = null;
            if (keyProto.hasEncryptedPrivateKey()) {
                Protos.EncryptedPrivateKey encryptedPrivateKeyProto = keyProto.getEncryptedPrivateKey();
                encryptedPrivateKey = new EncryptedPrivateKey(encryptedPrivateKeyProto.getInitialisationVector().toByteArray(),
                        encryptedPrivateKeyProto.getEncryptedPrivateKey().toByteArray());
            }

            byte[] pubKey = keyProto.hasPublicKey() ? keyProto.getPublicKey().toByteArray() : null;
            
            ECKey ecKey = null;
            if (walletType == EncryptionType.ENCRYPTED_SCRYPT_AES) {
                // If the key is an encrypted key construct an ECKey with the encrypted private key bytes.
                ecKey = new ECKey(encryptedPrivateKey, pubKey, encrypterDecrypter);
                
                // If any of the keys are encrypted, mark the wallet as encrypted.
                if (ecKey.isEncrypted()) {
                    isWalletCurrentlyEncrypted = true;
                }
            } else {
                ecKey = new ECKey(privKey, pubKey);
                             
            }
            ecKey.setCreationTimeSeconds((keyProto.getCreationTimestamp() + 500) / 1000);
            wallet.addKey(ecKey);           
        }
        
        wallet.setCurrentlyEncrypted(isWalletCurrentlyEncrypted);

        
        // Read all transactions and insert into the txMap.
        for (Protos.Transaction txProto : walletProto.getTransactionList()) {
            serializer.readTransaction(txProto, params);
        }

        // Update transaction outputs to point to inputs that spend them
        for (Protos.Transaction txProto : walletProto.getTransactionList()) {
            WalletTransaction wtx = serializer.connectTransactionOutputs(txProto);
            wallet.addWalletTransaction(wtx);
        }
        
        // Update the lastBlockSeenHash.
        if (!walletProto.hasLastSeenBlockHash()) {
            wallet.setLastBlockSeenHash(null);
        } else {
            wallet.setLastBlockSeenHash(byteStringToHash(walletProto.getLastSeenBlockHash()));
        }

        for (Protos.Extension extProto : walletProto.getExtensionList()) {
            if (extProto.getMandatory()) {
                throw new IllegalArgumentException("Did not understand a mandatory extension in the wallet");
            }
        }
        
        if (walletProto.hasMajorVersion()) {
            int majorVersion = walletProto.getMajorVersion();
            if (majorVersion == WalletMajorVersion.PROTOBUF.getWalletVersionAsInt()) {
                wallet.setMajorVersion(WalletMajorVersion.PROTOBUF);                
            } else {
                if (majorVersion == WalletMajorVersion.PROTOBUF_ENCRYPTED.getWalletVersionAsInt()) {
                    wallet.setMajorVersion(WalletMajorVersion.PROTOBUF_ENCRYPTED);                
                } else {
                    // Something from the future.
                    throw new WalletVersionException("Did not understand wallet major version of '" + majorVersion + "'");
                }   
            }
        } else {
            // Grandfather in as protobuf.2
            wallet.setMajorVersion(WalletMajorVersion.PROTOBUF);
        }
        
        int minorVersion = 0;
        if (walletProto.hasMinorVersion()) {
            minorVersion = walletProto.getMinorVersion();
        }
        wallet.setMinorVersion(minorVersion);
       
        return wallet;
    }


    private void readTransaction(Protos.Transaction txProto, NetworkParameters params) {
        Transaction tx = new Transaction(params);
        if (txProto.hasUpdatedAt()) {
            tx.setUpdateTime(new Date(txProto.getUpdatedAt()));
        }
        
        for (Protos.TransactionOutput outputProto : txProto.getTransactionOutputList()) {
            BigInteger value = BigInteger.valueOf(outputProto.getValue());
            byte[] scriptBytes = outputProto.getScriptBytes().toByteArray();
            TransactionOutput output = new TransactionOutput(params, tx, value, scriptBytes);
            tx.addOutput(output);
        }

        for (Protos.TransactionInput transactionInput : txProto.getTransactionInputList()) {
            byte[] scriptBytes = transactionInput.getScriptBytes().toByteArray();
            TransactionOutPoint outpoint = new TransactionOutPoint(params,
                    transactionInput.getTransactionOutPointIndex(),
                    byteStringToHash(transactionInput.getTransactionOutPointHash())
            );
            TransactionInput input = new TransactionInput(params, tx, scriptBytes, outpoint);
            if (transactionInput.hasSequence()) {
                input.setSequence(transactionInput.getSequence());
            }
            tx.addInput(input);
        }

        for (ByteString blockHash : txProto.getBlockHashList()) {
            tx.addBlockAppearance(byteStringToHash(blockHash));
        }

        if (txProto.hasLockTime()) {
            tx.setLockTime(txProto.getLockTime());
        }

        // Transaction should now be complete.
        Sha256Hash protoHash = byteStringToHash(txProto.getHash());
        Preconditions.checkState(tx.getHash().equals(protoHash),
                "Transaction did not deserialize completely: %s vs %s", tx.getHash(), protoHash);
        
        // If it is a duplicate, keep the newer.
        if (txMap.containsKey(txProto.getHash())) {
            Transaction txExisting = txMap.get(txProto.getHash());
            if (txExisting.getUpdateTime().after(new Date(txProto.getUpdatedAt()))) {
                // Existing transaction is newer. Keep it.
                log.debug("Wallet contained duplicate transaction %s, keeping the first and newer one", byteStringToHash(txProto.getHash()));
                return;
            } else {
                log.debug("Wallet contained duplicate transaction %s, using the second and newer one", byteStringToHash(txProto.getHash()));
            }
        }
        txMap.put(txProto.getHash(), tx);
    }

    private WalletTransaction connectTransactionOutputs(org.bitcoinj.wallet.Protos.Transaction txProto) {
        Transaction tx = txMap.get(txProto.getHash());
        WalletTransaction.Pool pool = WalletTransaction.Pool.valueOf(txProto.getPool().getNumber());
        for (int i = 0 ; i < tx.getOutputs().size() ; i++) {
            TransactionOutput output = tx.getOutputs().get(i);
            final Protos.TransactionOutput transactionOutput = txProto.getTransactionOutput(i);
            if (transactionOutput.hasSpentByTransactionHash()) {
                Transaction spendingTx = txMap.get(transactionOutput.getSpentByTransactionHash());
                final int spendingIndex = transactionOutput.getSpentByTransactionIndex();
                if (spendingTx != null ) {
                    TransactionInput input = spendingTx.getInputs().get(spendingIndex);
                    input.connect(output);
                }
            }
        }
        
        if (txProto.hasConfidence()) {
            Protos.TransactionConfidence confidenceProto = txProto.getConfidence();
            TransactionConfidence confidence = tx.getConfidence();
            readConfidence(tx, confidenceProto, confidence);
        }

        return new WalletTransaction(pool, tx);
    }

    private void readConfidence(Transaction tx, Protos.TransactionConfidence confidenceProto,
                                TransactionConfidence confidence) {
        // We are lenient here because tx confidence is not an essential part of the wallet.
        // If the tx has an unknown type of confidence, ignore.
        if (!confidenceProto.hasType()) {
            log.warn("Unknown confidence type for tx {}", tx.getHashAsString());
            return;
        }
        ConfidenceType confidenceType =
            TransactionConfidence.ConfidenceType.valueOf(confidenceProto.getType().getNumber());
        confidence.setConfidenceType(confidenceType);
        if (confidenceProto.hasAppearedAtHeight()) {
            if (confidence.getConfidenceType() != ConfidenceType.BUILDING) {
                log.warn("Have appearedAtHeight but not BUILDING for tx {}", tx.getHashAsString());
                return;
            }
            confidence.setAppearedAtChainHeight(confidenceProto.getAppearedAtHeight());
        }
        if (confidenceProto.hasOverridingTransaction()) {
            if (confidence.getConfidenceType() != ConfidenceType.OVERRIDDEN_BY_DOUBLE_SPEND) {
                log.warn("Have overridingTransaction but not OVERRIDDEN for tx {}", tx.getHashAsString());
                return;
            }
            Transaction overridingTransaction =
                txMap.get(confidenceProto.getOverridingTransaction());
            if (overridingTransaction == null) {
                log.warn("Have overridingTransaction that is not in wallet for tx {}", tx.getHashAsString());
                return;
            }
            confidence.setOverridingTransaction(overridingTransaction);
        }
    }
}
