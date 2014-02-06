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

import org.joda.money.Money;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyInfo;
import org.multibit.model.bitcoin.WalletTableData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.table.AbstractTableModel;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;



public class WalletTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012854496208L;

    private static final Logger log = LoggerFactory.getLogger(WalletTableModel.class);

    private ArrayList<String> headers;

    private ArrayList<WalletTableData> walletData;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    public WalletTableModel(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;

        createHeaders();

        walletData = this.bitcoinController.getModel().createWalletTableData(this.bitcoinController, this.bitcoinController.getModel().getActiveWalletFilename());
    }
    
    @Override
    public Class<?> getColumnClass(int columnIndex) {
        if (columnIndex == 3 || columnIndex == 4) {
            return Number.class;
        } else {
            return super.getColumnClass(columnIndex);
        }
    }

    @Override
    public int getColumnCount() {
        return headers.size();
    }

    @Override
    public int getRowCount() {
        return walletData.size();
    }

    public WalletTableData getRow(int row) {
        return walletData.get(row);
    }

    @Override
    public String getColumnName(int column) {
        return headers.get(column);
    }

    @Override
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
                return walletDataRow.getTransaction();
            } else {
                return null;
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
            // Amount in BTC
            BigInteger debitAmount = walletDataRow.getDebit();
            if (debitAmount != null && debitAmount.compareTo(BigInteger.ZERO) > 0) {
                return controller.getLocaliser().bitcoinValueToString(debitAmount.negate(), false, true);
            }

            BigInteger creditAmount = walletDataRow.getCredit();
            if (creditAmount != null) {
                return controller.getLocaliser().bitcoinValueToString(creditAmount, false, true);
            }
            
            return null;         
        case 4:
            // Amount in fiat
            if (walletDataRow.getDebit() != null  && walletDataRow.getDebit().compareTo(BigInteger.ZERO) > 0) {
                Money debitAmountFiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(walletDataRow.getDebit());
                if (debitAmountFiat != null) {
                    return CurrencyConverter.INSTANCE.getFiatAsLocalisedString(debitAmountFiat.negated(), false, false);
                }
            }

            Money creditAmountFiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(walletDataRow.getCredit());
            if (creditAmountFiat != null) {
                return CurrencyConverter.INSTANCE.getFiatAsLocalisedString(creditAmountFiat, false, false);
            }
            
            return "";
        default:
            return null;
        }
    }

    /**
     * Table model is read only.
     */
    @Override
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public void recreateWalletData() {
        // Recreate the wallet data as the underlying wallet has changed.
        walletData = this.bitcoinController.getModel().createActiveWalletData(this.bitcoinController);
        fireTableDataChanged();
    }

    public void createHeaders() {
        headers = new ArrayList<String>();
        for (int j = 0; j < WalletTableData.COLUMN_HEADER_KEYS.length; j++) {
            if ("sendBitcoinPanel.amountLabel".equals(WalletTableData.COLUMN_HEADER_KEYS[j])) {
                String header = controller.getLocaliser().getString(WalletTableData.COLUMN_HEADER_KEYS[j]) + " (" + controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel") + ")";
                headers.add(header);
            } else {
                headers.add(controller.getLocaliser().getString(WalletTableData.COLUMN_HEADER_KEYS[j]));                
            } 
        }
        
        // Add in the converted fiat, if appropriate
        if (CurrencyConverter.INSTANCE.isShowingFiat()) {
            CurrencyInfo currencyInfo = CurrencyConverter.INSTANCE.getCurrencyCodeToInfoMap().get(CurrencyConverter.INSTANCE.getCurrencyUnit().getCode());
            String currencySymbol = CurrencyConverter.INSTANCE.getCurrencyUnit().getCode();
            if (currencyInfo != null) {
                currencySymbol = currencyInfo.getCurrencySymbol();
            }
            String header = controller.getLocaliser().getString("sendBitcoinPanel.amountLabel") + " (" + currencySymbol + ")";
            headers.add(header);
        }
    }
}
