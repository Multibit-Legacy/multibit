package org.multibit.store;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Date;

import org.bitcoinj.wallet.Protos;
import org.bitcoinj.wallet.Protos.Wallet.EncryptionType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Sha256Hash;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionConfidence;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutPoint;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletTransaction;
import com.google.bitcoin.crypto.EncryptedPrivateKey;
import com.google.bitcoin.crypto.KeyCrypter;
import com.google.bitcoin.crypto.KeyCrypterException;
import com.google.bitcoin.crypto.KeyCrypterScrypt;
import com.google.bitcoin.store.WalletProtobufSerializer;
import com.google.common.base.Preconditions;
import com.google.protobuf.ByteString;


public class MultiBitWalletProtobufSerializer extends WalletProtobufSerializer {

    private static final Logger log = LoggerFactory.getLogger(MultiBitWalletProtobufSerializer.class);

    // Early version of name-value value for use in protecting encrypted wallets from being loaded
    // into earlier versions of MultiBit. Unfortunately I merged this into the MultiBit v0.4 code by mistake.
    // @deprecated replaced by ORG_MULTIBIT_WALLET_PROTECT_2
    public static final String ORG_MULTIBIT_WALLET_PROTECT = "org.multibit.walletProtect";

    public static final String ORG_MULTIBIT_WALLET_PROTECT_2 = "org.multibit.walletProtect.2";

    public MultiBitWalletProtobufSerializer() {
        super();
        this.setWalletExtensionSerializer(new MultiBitWalletExtensionSerializer());
    }
    
    /**
     * Converts the given wallet to the object representation of the protocol buffers. This can be modified, or
     * additional data fields set, before serialization takes place.
     */
    public Protos.Wallet walletToProto(Wallet wallet) {
        Protos.Wallet.Builder walletBuilder = Protos.Wallet.newBuilder();
        walletBuilder.setNetworkIdentifier(wallet.getNetworkParameters().getId());
        if (wallet.getDescription() != null) {
            walletBuilder.setDescription(wallet.getDescription());
        }
        
        for (WalletTransaction wtx : wallet.getWalletTransactions()) {
            Protos.Transaction txProto = makeTxProto(wtx);
            walletBuilder.addTransaction(txProto);
        }
        
        for (ECKey key : wallet.getKeys()) {
            Protos.Key.Builder buf = Protos.Key.newBuilder().setCreationTimestamp(key.getCreationTimeSeconds() * 1000)
                                                         // .setLabel() TODO
                                                            .setType(Protos.Key.Type.ORIGINAL);
            if (key.getPrivKeyBytes() != null)
                buf.setPrivateKey(ByteString.copyFrom(key.getPrivKeyBytes()));

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
        KeyCrypter keyCrypter = wallet.getKeyCrypter();
        if (keyCrypter == null) {
            // The wallet is unencrypted.
            walletBuilder.setEncryptionType(EncryptionType.UNENCRYPTED);
        } else {
            // The wallet is encrypted.
            walletBuilder.setEncryptionType(keyCrypter.getEncryptionType());
            if (keyCrypter instanceof KeyCrypterScrypt) {
                walletBuilder.setEncryptionType(EncryptionType.ENCRYPTED_SCRYPT_AES);
                KeyCrypterScrypt keyCrypterScrypt = (KeyCrypterScrypt) keyCrypter;
                walletBuilder.setEncryptionParameters(keyCrypterScrypt.getScryptParameters());
            } else {
                // Some other form of encryption has been specified that we do not know how to persist.
                throw new RuntimeException("The wallet has encryption of type '" + keyCrypter.getEncryptionType() + "' but this WalletProtobufSerializer does not know how to persist this.");
            }
        }

        // Populate the wallet version.
        if (wallet.getVersion() != null) {
            walletBuilder.setVersion(wallet.getVersion().getWalletVersionAsInt());
        }

        Collection<Protos.Extension> extensions = helper.getExtensionsToWrite(wallet);
        for(Protos.Extension ext : extensions) {
            walletBuilder.addExtension(ext);
        }

        return walletBuilder.build();
    }
    
    /**
     * Parses a wallet from the given stream. The stream is expected to contain a binary serialization of a 
     * {@link Protos.Wallet} object.<p>
     *     
     * If the stream is invalid or the serialized wallet contains unsupported features, 
     * {@link IllegalArgumentException} is thrown.
     * @throws KeyCrypterException 
     *
     */
    public Wallet readWallet(InputStream input) throws IOException, KeyCrypterException {
        // TODO: This method should throw more specific exception types than IllegalArgumentException.
        Protos.Wallet walletProto = parseToProto(input);

        // System.out.println(TextFormat.printToString(walletProto));

        // Read the encryption type to see if the wallet is encrypted or not.
        // If not specified it is unencrypted.
        EncryptionType walletEncryptionType = EncryptionType.UNENCRYPTED;
        
        if (walletProto.hasEncryptionType()) {
            walletEncryptionType = walletProto.getEncryptionType();
        }
        KeyCrypter keyCrypter = null;
        
        if (walletEncryptionType == EncryptionType.ENCRYPTED_SCRYPT_AES) {
            // Read the scrypt parameters that specify how encryption and decryption is performed.
            if (walletProto.hasEncryptionParameters()) {
                Protos.ScryptParameters encryptionParameters = walletProto.getEncryptionParameters();
                keyCrypter = new KeyCrypterScrypt(encryptionParameters);
            }
        }

        NetworkParameters params = NetworkParameters.fromID(walletProto.getNetworkIdentifier());
        
        // Create the wallet - note an unencrypted wallet is passed in a null keyCrypter to indicate this.
        Wallet wallet = helper.newWallet(params, keyCrypter);
        
        if (walletProto.hasDescription()) {
            wallet.setDescription(walletProto.getDescription());
        }
        
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
            if (keyCrypter != null && keyCrypter.getEncryptionType() != EncryptionType.UNENCRYPTED) {
                // If the key is encrypted construct an ECKey using the encrypted private key bytes.
                ecKey = new ECKey(encryptedPrivateKey, pubKey, keyCrypter);
            } else {
                // Construct an unencrypted private key.
                ecKey = new ECKey(privKey, pubKey);           
            }
            ecKey.setCreationTimeSeconds((keyProto.getCreationTimestamp() + 500) / 1000);
            wallet.addKey(ecKey);
        }

        // Read all transactions and insert into the txMap.
        for (Protos.Transaction txProto : walletProto.getTransactionList()) {
            readTransaction(txProto, params);
        }

        // Update transaction outputs to point to inputs that spend them
        for (Protos.Transaction txProto : walletProto.getTransactionList()) {
            WalletTransaction wtx = connectTransactionOutputs(txProto);
            wallet.addWalletTransaction(wtx);
        }

        // Update the lastBlockSeenHash.
        if (!walletProto.hasLastSeenBlockHash()) {
            wallet.setLastBlockSeenHash(null);
        } else {
            wallet.setLastBlockSeenHash(byteStringToHash(walletProto.getLastSeenBlockHash()));
        }

        for (Protos.Extension extProto : walletProto.getExtensionList()) {
            helper.readExtension(wallet, extProto);
        }

        if (walletProto.hasVersion()) {
            int version = walletProto.getVersion();
            if (version == MultiBitWalletVersion.PROTOBUF.getWalletVersionAsInt()) {
                wallet.setVersion(MultiBitWalletVersion.PROTOBUF);
            } else {
                if (version == MultiBitWalletVersion.PROTOBUF_ENCRYPTED.getWalletVersionAsInt()) {
                    wallet.setVersion(MultiBitWalletVersion.PROTOBUF_ENCRYPTED);  
                } else {
                    // Something from the future.
                    throw new WalletVersionException("Did not understand wallet version of '" + version + "'");
                }
            }
        } else {
            // Grandfather in as protobuf.2
            wallet.setVersion(MultiBitWalletVersion.PROTOBUF);
        }

        return wallet;
    }

    protected void readTransaction(Protos.Transaction txProto, NetworkParameters params) {
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
                input.setSequenceNumber(transactionInput.getSequence());
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
        // (This code is is here because some old MultiBit serialised wallets had the same tx appearing twice and the wallets would not load).
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
    
    /**
     * Formats the given Wallet to the given output stream in protocol buffer format.
     * Add a mandatory extension so that it will not be loaded by older versions.
     */
    public void writeWalletWithMandatoryExtension(Wallet wallet, OutputStream output) throws IOException {
        Protos.Wallet walletProto = walletToProto(wallet);
        Protos.Wallet.Builder walletBuilder = Protos.Wallet.newBuilder(walletProto);
        Protos.Extension.Builder extensionBuilder = Protos.Extension.newBuilder().setId(ORG_MULTIBIT_WALLET_PROTECT_2).setData(ByteString.copyFrom(new byte[0x01])).setMandatory(true);
        walletBuilder.addExtension(extensionBuilder);

        Protos.Wallet walletProtoWithMandatory = walletBuilder.build();
        walletProtoWithMandatory.writeTo(output);
    }
    
    protected WalletTransaction connectTransactionOutputs(org.bitcoinj.wallet.Protos.Transaction txProto) {
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
}