package org.multibit.model;

public class Item {
    private String itemName;
    private Object originalValue;
    private Object newValue;
    
    public Item(String itemName) {
        this.itemName = itemName;
    }

    public Object getOriginalValue() {
        return originalValue;
    }

    public void setOriginalValue(Object originalValue) {
        this.originalValue = originalValue;
    }

    public Object getNewValue() {
        return newValue;
    }

    public void setNewValue(Object newValue) {
        this.newValue = newValue;
    }

    public String getItemName() {
        return itemName;
    }
}
