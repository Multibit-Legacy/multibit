package org.multibit.viewsystem.swing.view;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.WalletInfo;

import javax.swing.table.DefaultTableModel;
import java.util.Vector;

public class AddressBookTableModel extends DefaultTableModel {

    private static final long serialVersionUID = -937886012851116208L;

    // TODO Consider an ArrayList if possible
    private Vector<String> headers = new Vector<String>();

    private final String[] tableHeaderKeys = new String[] { "addressBookTableModel.labelColumnHeader",
            "addressBookTableModel.addressColumnHeader" };

    private boolean isReceiving;

    private MultiBitController controller;

    public AddressBookTableModel(MultiBitController controller, boolean isReceiving) {
        this.controller = controller;
      for (String tableHeaderKey : tableHeaderKeys) {
        headers.add(controller.getLocaliser().getString(tableHeaderKey));
      }

      this.isReceiving = isReceiving;
    }

    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    public int getRowCount() {
        if (controller == null) {
            return 0;
        }
        WalletInfo walletInfo = controller.getModel().getWalletInfo();
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

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        WalletInfo walletInfo = controller.getModel().getWalletInfo();

        if (walletInfo == null) {
            return null;
        }

        Vector<AddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        AddressBookData[] addressesArray = addresses.toArray(new AddressBookData[addresses.size()]);
        AddressBookData addressBookData = null;
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
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    /**
     * find a row, given an address
     * @param address The address
     * @param isReceiving true if receiving
     * @return The row for the address
     */
    public int findRowByAddress(String address, boolean isReceiving) {
        if (address == null) {
            return -1;
        }
        WalletInfo walletInfo = controller.getModel().getWalletInfo();
        if (walletInfo == null) {
            return -1;
        }

        Vector<AddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        int row = 0;
        if (addresses != null) {
            for (AddressBookData loopAddress : addresses) {
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
     * given a row, return the AddressBookData on this row
     */
    public AddressBookData getAddressBookDataByRow(int row, boolean isReceiving) {
        WalletInfo walletInfo = controller.getModel().getWalletInfo();
        if (walletInfo == null) {
            return null;
        }

        Vector<AddressBookData> addresses;
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
     * set a AddressBookData into a row
     */
    public void setAddressBookDataByRow(AddressBookData addressBookData, int row, boolean isReceiving) {
        WalletInfo walletInfo = controller.getModel().getWalletInfo();
        if (walletInfo == null) {
            return;
        }

        Vector<AddressBookData> addresses;
        if (isReceiving) {
            addresses = walletInfo.getReceivingAddresses();
        } else {
            addresses = walletInfo.getSendingAddresses();
        }

        if (addresses != null && addresses.size() > row) {
            addresses.set(row, addressBookData);
            fireTableRowsUpdated(row, row);
        }
    }

    @Override
    public boolean isCellEditable(int row, int column) {
        // all cells false
        return false;
    }
}
