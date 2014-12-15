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
package org.multibit.viewsystem.swing.view.models;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.table.AbstractTableModel;
import org.joda.money.Money;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.viewsystem.swing.view.panels.DraftsPanel;

public class DraftsTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012851116208L;

    private final String[] tableHeaderKeys;
    private final BitcoinController bitcoinController;
        
    protected Money parsedAmountBTC = null;
    protected Money parsedAmountFiat = null;

    public DraftsTableModel(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        tableHeaderKeys = new String[] { "Adress","Label","Amount (BTC)" , "Amount ($)"};

    }
        private ArrayList draftData(){
        ArrayList<String> array = new ArrayList();
        String address = "";
        WalletInfoData addressBook = bitcoinController.getModel().getActiveWalletWalletInfo();
            if (addressBook != null) {
                ArrayList<WalletAddressBookData> receivingAddresses = addressBook.getReceivingAddresses();
                if (receivingAddresses != null) {
                    if (receivingAddresses.iterator().hasNext()) {
                        WalletAddressBookData addressBookData = receivingAddresses.iterator().next();
                        if (addressBookData != null) {
                            address = addressBookData.getAddress();
                            bitcoinController.getModel().setActiveWalletPreference(BitcoinModel.RECEIVE_ADDRESS, address);
                        }
                    }
                }
            }
        BufferedReader br;
        try {
            br = new BufferedReader(new FileReader(address + "_Drafts.txt"));
            String line;
                try {
                    line = br.readLine();
            while (line != null) {
                array.add(line);
                line = br.readLine();
            }
        } finally {
            br.close();
        }
        } catch (IOException ex) {
            Logger.getLogger(DraftsPanel.class.getName()).log(Level.SEVERE, null, ex);
        }
        return array;
    }
    @Override
    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    @Override
    public int getRowCount() {
        ArrayList array = draftData();
        return array.size();
    }

    @Override
    public String getColumnName(int column) {
        return tableHeaderKeys[column];
    }

    @Override
    public String getValueAt(int row, int column) {
        ArrayList<String> dataArray = draftData();
        if(row<dataArray.size()){
            String strRow = dataArray.get(row);
            String[] parts = strRow.split("////");
            if(column<parts.length){
                return parts[column];
            }
            if(column == 3){
                CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTC(parts[2]);                    
                parsedAmountBTC = converterResult.getBtcMoney();
                parsedAmountFiat = CurrencyConverter.INSTANCE.convertFromBTCToFiat(parsedAmountBTC.getAmount().toBigInteger());
                return CurrencyConverter.INSTANCE.getFiatAsLocalisedString(parsedAmountFiat, false, false); 
            }
        }
        return null;
    }

    /**
     * table model is read only
     */
    @Override
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public int findRowByAddress(String address) {
        if (address == null) {
            return -1;
        }
        ArrayList<String> dataArray = draftData();
        String addresses;
        int i =0;
        while(i<dataArray.size()){
            String row = dataArray.get(i);
            String[] parts = row.split("////");
            addresses = parts[1];
            if(address.equals(addresses)){
                return i;
            }
            i++;
        }
        return -1;
    }

    @Override
    public boolean isCellEditable(int row, int column) {
        // all cells false
        return false;
    }
}
