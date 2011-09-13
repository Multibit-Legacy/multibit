package org.multibit.model;

import java.util.HashMap;
import java.util.Map;

/**
 * a class representing data, normally form data to be used by actions
 * @author jim
 *
 */
public class Data {
    // items of data, keyed by item name
    private Map<String, Item> items;
    private DataType dataType;
    
    public Data() {
        items = new HashMap<String, Item>();
    }
    
    public Item getItem(String itemName) {
        return items.get(itemName);
    }
    
    public void addItem(String itemName, Item item) {
        items.put(itemName, item);
    }
    
    public DataType getDataType() {
        return dataType;
    }
    
    public void setDataType(DataType dataType) {
        this.dataType = dataType;
    }    
}
