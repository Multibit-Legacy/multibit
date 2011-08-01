package org.multibit.model;

/**
 * class used to store the data in the table in a quick to access form
 */
@SuppressWarnings("rawtypes")
public class AddressBookData implements Comparable {
    String label;
    String address;

    public AddressBookData(String label, String address) {
        this.label = label;
        this.address = address;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((address == null) ? 0 : address.hashCode());
        result = prime * result + ((label == null) ? 0 : label.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AddressBookData other = (AddressBookData) obj;
        if (address == null) {
            if (other.address != null)
                return false;
        } else if (!address.equals(other.address))
            return false;
        if (label == null) {
            if (other.label != null)
                return false;
        } else if (!label.equals(other.label))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "AddressBookData [label=" + label + ", address=" + address + "]";
    }

    public int compareTo(Object other) {
        if (other instanceof AddressBookData) {
            return (label + "").compareTo(((AddressBookData)other).label);
        } else {
            return 0;
        }
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }
    
    
}