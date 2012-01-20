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
