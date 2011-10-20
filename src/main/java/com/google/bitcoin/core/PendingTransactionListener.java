package com.google.bitcoin.core;

/**
 * interface for interested parties to hook up to PeerGroup if they want to be
 * notified of incoming zero confirmation transactions
 * 
 * note: all zero confirmation transactions will be passed to the listener
 * so you want to trim down to the transactions you are interested in 
 * 
 * @author jim burton
 *
 */
public interface PendingTransactionListener {
    
    /*
     * Call back to process a zero confirmation pending transaction
     */
    public void processPendingTransaction(Transaction transaction);
}
