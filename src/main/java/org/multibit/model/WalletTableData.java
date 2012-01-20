/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.model;

import com.google.bitcoin.core.Transaction;

import java.math.BigInteger;
import java.util.Date;

/**
 * class used to store the data in the table in a quick to access form
 * for use in tables mainly
 */
public class WalletTableData {
    
    /**
     * keys that give column header text for output formatting
     * TODO Exposing mutable state consider cloning
     */
    public static final String[] COLUMN_HEADER_KEYS = new String[] { "walletData.statusText",
            "walletData.dateText", "walletData.descriptionText",
            "walletData.debitText", "walletData.creditText" };

    private Transaction transaction;
    
    /**
     * the height of the block this transaction appears in
     */
    private int height;

    // TODO Consider using Joda Time (java.util.Date is obsolete)
    private Date date;
    private String description;
    private BigInteger debit;
    private BigInteger credit;

    public WalletTableData(Transaction transaction, int height, Date date, String description,
            BigInteger debit, BigInteger credit) {
        this.transaction = transaction;
        this.height = height;
        // Avoid exposing internal state
        this.date = new Date(date.getTime());
        this.description = description;
        this.debit = debit;
        this.credit = credit;
    }

    public WalletTableData(Transaction transaction) {
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
        // Avoids exposing internal state
        return new Date(date.getTime());
    }

    public void setDate(Date date) {
        this.date = new Date(date.getTime());
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
