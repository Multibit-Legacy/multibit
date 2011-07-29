package org.multibit.model;

import java.math.BigInteger;
import java.util.Date;

import com.google.bitcoin.core.Transaction;

/**
 * class used to store the data in the table in a quick to access form
 * TODO - this will be replaced by something driven by the wallet
 */
public class WalletData {
    
    /**
     * keys that give column header text for output formatting
     */
    public static final String[] COLUMN_HEADER_KEYS = new String[] { "walletData.statusText",
            "walletData.dateText", "walletData.descriptionText",
            "walletData.debitText", "walletData.creditText" };

    private Transaction transaction;
    
    /**
     * the height of the block this transaction appears in
     */
    private int height;
    
    private Date date;
    private String description;
    private BigInteger debit;
    private BigInteger credit;

    public WalletData(Transaction transaction, int height, Date date, String description,
            BigInteger debit, BigInteger credit) {
        this.transaction = transaction;
        this.height = height;
        this.date = date;
        this.description = description;
        this.debit = debit;
        this.credit = credit;
    }

    public WalletData(Transaction transaction) {
        this.transaction = transaction;
    }

    public Transaction getTransaction() {
        return transaction;
    }

    public void setTransaction(Transaction transaction) {
        this.transaction = transaction;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigInteger getDebit() {
        return debit;
    }

    public void setDebit(BigInteger debit) {
        this.debit = debit;
    }

    public BigInteger getCredit() {
        return credit;
    }

    public void setCredit(BigInteger credit) {
        this.credit = credit;
    }
}
