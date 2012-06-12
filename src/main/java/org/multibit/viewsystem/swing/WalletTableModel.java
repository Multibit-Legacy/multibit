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
package org.multibit.viewsystem.swing;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;

import javax.swing.table.AbstractTableModel;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletTableData;

import com.google.bitcoin.core.TransactionConfidence.ConfidenceType;

public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private ArrayList<String> headers;

    private ArrayList<WalletTableData> walletData;

    /**
     * The MultiBit model.
     */
    private MultiBitModel multiBitModel;

    private MultiBitController controller;

    public WalletTableModel(MultiBitController controller) {
        this.multiBitModel = controller.getModel();
        this.controller = controller;

        createHeaders();

        walletData = multiBitModel.createWalletData(controller.getModel().getActiveWalletFilename());
    }

    public int getColumnCount() {
        return WalletTableData.COLUMN_HEADER_KEYS.length;
    }

    public int getRowCount() {
        return walletData.size();
    }

    public WalletTableData getRow(int row) {
        return walletData.get(row);
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletTableData walletDataRow = null;
        if (row >= 0 && row < walletData.size()) {
            walletDataRow = walletData.get(row);
        }
        if (walletDataRow == null) {
            return null;
        }

        switch (column) {
        case 0: {
            if (walletDataRow.getTransaction() != null && walletDataRow.getTransaction().getConfidence() != null) {
                return walletDataRow.getTransaction().getConfidence();
            } else {
                return  ConfidenceType.UNKNOWN;
            }
        }
        case 1: {
            if (walletDataRow.getDate() == null) {
                return new Date(0); // the earliest date (for sorting)
            } else {
                return walletDataRow.getDate();
            }
        }
        case 2:
            return walletDataRow.getDescription();
        case 3:
            BigInteger debitAmount = walletDataRow.getDebit();
            if (debitAmount == null) {
                return null;
            } else {
                return controller.getLocaliser().bitcoinValueToString4(debitAmount, false, true);
            }
        case 4:
            BigInteger creditAmount = walletDataRow.getCredit();
            if (creditAmount == null) {
                return null;
            } else {
                return controller.getLocaliser().bitcoinValueToString4(creditAmount, false, true);
            }
        default:
            return null;
        }
    }

    /**
     * table model is read only
     */
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public void recreateWalletData() {
        // recreate the wallet data as the underlying wallet has changed
        //log.debug("Updating walletTableModel for file '" + controller.getModel().getActiveWalletFilename() + "'");
        walletData = multiBitModel.createWalletData(controller.getModel().getActiveWalletFilename());
        //log.debug("walletTableModel now has " + walletData.size() + " rows");
        fireTableDataChanged();
    }

    public void createHeaders() {
        headers = new ArrayList<String>();
        for (int j = 0; j < WalletTableData.COLUMN_HEADER_KEYS.length; j++) {
            headers.add(controller.getLocaliser().getString(WalletTableData.COLUMN_HEADER_KEYS[j]));
        }
    }
}
