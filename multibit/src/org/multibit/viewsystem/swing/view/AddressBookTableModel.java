package org.multibit.viewsystem.swing.view;

import java.util.SortedSet;
import java.util.Vector;

import javax.swing.table.DefaultTableModel;

import org.multibit.Localiser;
import org.multibit.model.AddressBook;
import org.multibit.model.AddressBookData;

public class AddressBookTableModel extends DefaultTableModel {

    private static final long serialVersionUID = -937886012851116208L;

    private Vector<String> headers = new Vector<String>();

    private final String[] tableHeaderKeys = new String[] {
            "addressBookTableModel.labelColumnHeader", "addressBookTableModel.addressColumnHeader" };

    private AddressBook addressBook;
    private boolean isReceiving;

    public AddressBookTableModel(Localiser localiser, AddressBook addressBook, boolean isReceiving) {
        for (int j = 0; j < tableHeaderKeys.length; j++) {
            headers.add(localiser.getString(tableHeaderKeys[j]));
        }
        
        this.addressBook = addressBook;
        this.isReceiving = isReceiving;
    }

    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    public int getRowCount() {
        if (isReceiving) {
            if (addressBook != null && addressBook.getReceivingAddresses() != null) {
                return addressBook.getReceivingAddresses().size();
            } else {
                return 0;
            }
        } else {
            if (addressBook != null && addressBook.getSendingAddresses() != null) {
                return addressBook.getSendingAddresses().size();
            } else {
                return 0;
            }
        }
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        SortedSet<AddressBookData> addresses;
        if (isReceiving) {
            addresses = addressBook.getReceivingAddresses();
        } else {
            addresses = addressBook.getSendingAddresses();
        }

        AddressBookData[] addressesArray = (AddressBookData[]) addresses.toArray(new AddressBookData[addresses.size()]);
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
     */
    public int findRowByAddress(String address, boolean isReceiving) {
        if (address == null) {
            return -1;
        }
        SortedSet<AddressBookData> addresses;
        if (isReceiving) {
            addresses = addressBook.getReceivingAddresses();
        } else {
            addresses = addressBook.getSendingAddresses();
        }
        
        int row = 0;
        if (addresses != null) {
            for (AddressBookData loopAddress : addresses) {
                if (address.equals(loopAddress.getAddress())) {
                    // select this row in the table
                    return row;
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
        SortedSet<AddressBookData> addresses;
        if (isReceiving) {
            addresses = addressBook.getReceivingAddresses();
        } else {
            addresses = addressBook.getSendingAddresses();
        }
        
        int loopRow = 0;
        if (addresses != null) {
            for (AddressBookData loopAddress : addresses) {
                if (loopRow == row) {
                    // return this row
                    //System.out.println("AddressBookModel#getAddressBookDataByRow, row = " + row + ", isReceiving = " + isReceiving + ", addressBookData = " + loopAddress);
                    return loopAddress;
                }
                loopRow++;
            }
        }
        return null;
    }
}
