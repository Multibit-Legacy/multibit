/**
 * Copyright 2012 multibit.org
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
 */package org.multibit.file;

import java.util.Date;

import com.google.bitcoin.core.ECKey;

/**
 * POFO containing an ECKey and Date
 * @author jim
 *
 */
public class PrivateKeyAndDate {
    private ECKey key;
    private Date date;
 
    public PrivateKeyAndDate() {
        
    }
    
    public PrivateKeyAndDate(ECKey key, Date date) {
        super();
        this.key = key;
        this.date = date;
    }
 
    @Override
    public String toString() {
        return "PrivateKeyAndDate [key=" + key + ", date=" + date + "]";
    }
    
    public ECKey getKey() {
        return key;
    }
    public void setKey(ECKey key) {
        this.key = key;
    }
    public Date getDate() {
        return date;
    }
    public void setDate(Date date) {
        this.date = date;
    }
    

}
