package com.google.bitcoin.store;

import java.math.BigInteger;
import java.util.Date;

import org.bitcoinj.wallet.Protos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.NetworkParameters;
import com.google.bitcoin.core.Sha256Hash;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutPoint;
import com.google.bitcoin.core.TransactionOutput;
import com.google.common.base.Preconditions;
import com.google.protobuf.ByteString;


public class MultiBitWalletProtobufSerializer extends WalletProtobufSerializer {

    private static final Logger log = LoggerFactory.getLogger(MultiBitWalletProtobufSerializer.class);

    public MultiBitWalletProtobufSerializer() {
        super();
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

}
