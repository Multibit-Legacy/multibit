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

import java.util.ArrayList;

import javax.swing.table.DefaultTableModel;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.bitcoin.WalletAddressBookData;
import org.multibit.model.bitcoin.WalletInfoData;

public class AddressBookTableModel extends DefaultTableModel {

    private static final long serialVersionUID = -937886012851116208L;

    private ArrayList<String> headers = new ArrayList<String>();

    private final String[] tableHeaderKeys = new String[] { "addressBookTableModel.labelColumnHeader",
            "addressBookTableModel.addressColumnHeader" };

    private boolean isReceiving;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    public AddressBookTableModel(BitcoinController bitcoinController, boolean isReceiving) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        for (String tableHeaderKey : tableHeaderKeys) {
            headers.add(controller.getLocaliser().getString(tableHeaderKey));
        }

        this.isReceiving = isReceiving;
    }

    @Override
    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    @Override
    public int getRowCount() {
        if (controller == null) {
            return 0;
        }
        WalletInfoData walletInfo = this.bitcoinController.getModel().getActiveWalletWalletInfo();
        if (isReceiving) {
            if (walletInfo != null && walletInfo.getReceivingAddresses() != null) {
                return walletInfo.getReceivingAddresses().size();
            } else {
                return 0;
            }
        } else {
            if (walletInfo != null && walletInfo.getSendingAddresses() != null) {
                return walletInfo.getSendingAddresses().size();
            } else {
                return 0;
            }
        }
    }

    @Override
    public String getColumnName(int column) {
        return headers.get(column);
    }

    @Override
    public Object getValueAt(int row, int column) {
        WalletInfoData walletInfo = this.bitcoinController.getModel().getActiveWalletWalletInfo();

        if (walletInfo == null) {
            return null;
        }

        ArrayList<WalletAddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        WalletAddressBookData[] addressesArray = addresses.toArray(new WalletAddressBookData[addresses.size()]);
        WalletAddressBookData addressBookData = null;
        if (row >= 0 && row < addresses.size()) {
            addressBookData = addressesArray[row];
        }

        if (addressBookData == null) {
            return null;
        }

        switch (column) {
        case 0:
            return addressBookData.getLabel();
        case 1:
            return addressBookData.getAddress();
        default:
            return null;
        }
    }

    /**
     * table model is read only
     */
    @Override
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    /**
     * find a row, given an address
     * 
     * @param address
     *            The address
     * @param isReceiving
     *            true if receiving
     * @return The row for the address
     */
    public int findRowByAddress(String address, boolean isReceiving) {
        if (address == null) {
            return -1;
        }
        WalletInfoData walletInfo = this.bitcoinController.getModel().getActiveWalletWalletInfo();
        if (walletInfo == null) {
            return -1;
        }

        ArrayList<WalletAddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        int row = 0;
        if (addresses != null) {
            for (WalletAddressBookData loopAddress : addresses) {
                if (loopAddress != null) {
                    if (address.equals(loopAddress.getAddress())) {
                        // select this row in the table
                        return row;
                    }
                }
                row++;
            }
        }
        return -1;
    }

    /**
     * given a row, return the WalletAddressBookData on this row
     */
    public WalletAddressBookData getAddressBookDataByRow(int row, boolean isReceiving) {
        WalletInfoData walletInfo = this.bitcoinController.getModel().getActiveWalletWalletInfo();
        if (walletInfo == null) {
            return null;
        }

        ArrayList<WalletAddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        if (addresses != null && addresses.size() > row) {
            return addresses.get(row);
        }
        return null;
    }

    /**
     * set a WalletAddressBookData into a row
     */
    public void setAddressBookDataByRow(WalletAddressBookData addressBookData, int row, boolean isReceiving) {
        WalletInfoData walletInfo = this.bitcoinController.getModel().getActiveWalletWalletInfo();
        if (walletInfo == null) {
            return;
        }

        ArrayList<WalletAddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        if (addresses != null && addresses.size() > row) {
            addresses.set(row, addressBookData);

            fireTableDataChanged();
        }
    }

    @Override
    public boolean isCellEditable(int row, int column) {
        // all cells false
        return false;
    }
}
