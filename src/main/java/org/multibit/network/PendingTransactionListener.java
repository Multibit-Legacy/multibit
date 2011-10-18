package org.multibit.network;

import com.google.bitcoin.core.Transaction;

public interface PendingTransactionListener {
    public void processPendingTransaction(Transaction transaction);
}
