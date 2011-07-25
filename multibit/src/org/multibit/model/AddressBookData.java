package org.multibit.model;

/**
 * class used to store the data in the table in a quick to access form
 */
public class AddressBookData {
    String label;
    String address;

    public AddressBookData(String label, String address) {
        this.label = label;
        this.address = address;
    }
}