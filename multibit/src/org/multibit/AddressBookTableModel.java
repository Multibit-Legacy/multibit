package org.multibit;

import java.util.Vector;

import javax.swing.table.AbstractTableModel;

public class AddressBookTableModel extends AbstractTableModel {

    private static final long serialVersionUID = -937886012851116208L;

    private Vector<String> headers = new Vector<String>();

    private final String[] tableHeaderKeys = new String[] {
            "addressBookTableModel.labelColumnHeader", "addressBookTableModel.addressColumnHeader" };

    private Vector<AddressBookData> addressBookDataVector;

    public AddressBookTableModel(Localiser localiser, boolean isReceiving) {
        for (int j = 0; j < tableHeaderKeys.length; j++) {
            headers.add(localiser.getString(tableHeaderKeys[j]));
        }

        if (isReceiving) {
            addressBookDataVector = createFakeReceivingAddressBookData();
        } else {
            addressBookDataVector = createFakeSendingAddressBookData();
        }
    }

    public int getColumnCount() {
        return tableHeaderKeys.length;
    }

    public int getRowCount() {
        return addressBookDataVector.size();
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        AddressBookData addressBookData = null;
        if (row >= 0 && row < addressBookDataVector.size()) {
            addressBookData = addressBookDataVector.get(row);
        }
        if (addressBookData == null) {
            return null;
        }

        switch (column) {
        case 0:
            return addressBookData.label;
        case 1:
            return addressBookData.address;
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

    private Vector<AddressBookData> createFakeReceivingAddressBookData() {
        addressBookDataVector = new Vector<AddressBookData>();

        // fake data
        AddressBookData addressBookData;

        addressBookData = new AddressBookData("Mt Gox account", "1x256f9e9");
        addressBookDataVector.add(addressBookData);

        addressBookData = new AddressBookData("Jim's Amazon account", "1xqs");
        addressBookDataVector.add(addressBookData);

        addressBookData = new AddressBookData("Google dividend", "15x31");
        addressBookDataVector.add(addressBookData);

        return addressBookDataVector;
    }

    private Vector<AddressBookData> createFakeSendingAddressBookData() {
        addressBookDataVector = new Vector<AddressBookData>();

        // fake data
        AddressBookData addressBookData;
            addressBookData = new AddressBookData("Jose Alvaro","1x3f45ed");
            addressBookDataVector.add(addressBookData);

            addressBookData = new AddressBookData("Joseph Jacks", "1xq90");
            addressBookDataVector.add(addressBookData);

            addressBookData = new AddressBookData("Michelle Jones", "1j8s2");
            addressBookDataVector.add(addressBookData);

        return addressBookDataVector;
    }

    /**
     * class used to store the data in the table in a quick to access form
     */
    class AddressBookData {
        String label;
        String address;

        AddressBookData(String label, String address) {
            this.label = label;
            this.address = address;
        }
    }
}
