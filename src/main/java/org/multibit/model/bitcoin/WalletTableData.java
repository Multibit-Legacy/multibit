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
package org.multibit.model.bitcoin;

import java.math.BigInteger;
import java.util.Date;

import com.google.bitcoin.core.Transaction;

/**
 * Class used to store the data in the Transactions table in a quick to access data form.
 */
public class WalletTableData {
    
    /**
     * Keys that give column header text for output formatting.
     * TODO Exposing mutable state consider cloning
     */
    public static final String[] COLUMN_HEADER_KEYS = new String[] { "walletData.statusText",
            "walletData.dateText", "walletData.descriptionText",
            "sendBitcoinPanel.amountLabel" };

    private Transaction transaction;
    
    /**
     * The height of the block this transaction appears in.
     */
    private int height;

    // TODO Consider using Joda Time (java.util.Date is obsolete)
    private Date date;
    private String description;
    private BigInteger debit;
    private BigInteger credit;

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
        if (date == null) {
            return null;
        } else {
            return new Date(date.getTime());
        }
    }

    public void setDate(Date date) {
        if (date == null) {
            this.date = null;
        } else {
            this.date = new Date(date.getTime());
        }
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
