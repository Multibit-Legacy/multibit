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
 */
package org.multibit.file;

import java.util.Date;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Utils;

/**
 * POJO containing an ECKey and Date
 * 
 * @author jim
 * 
 */
public class PrivateKeyAndDate {
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((date == null) ? 0 : date.hashCode());
        result = prime * result + ((key == null) ? 0 : Utils.bytesToHexString(key.getPrivKeyBytes()).hashCode());
        result = prime * result + ((key == null) ? 0 : Utils.bytesToHexString(key.getPubKey()).hashCode());
        result = prime * result + ((key == null) ? 0 : Utils.bytesToHexString(key.getEncryptedPrivateKey().getEncryptedBytes()).hashCode());
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
        PrivateKeyAndDate other = (PrivateKeyAndDate) obj;
        if (date == null) {
            if (other.date != null)
                return false;
        } else if (other.date == null) {
            return false;
        } else if (date.getTime() != other.date.getTime()) {
            return false;
        }
        if (key == null) {
            if (other.key != null)
                return false;
        } else if (!Utils.bytesToHexString(key.getPrivKeyBytes()).equals(Utils.bytesToHexString(other.key.getPrivKeyBytes()))) {
            return false;
        } else if (!Utils.bytesToHexString(key.getPubKey()).equals(Utils.bytesToHexString(other.key.getPubKey()))) {
            return false;
        } else if (!Utils.bytesToHexString(key.getEncryptedPrivateKey().getEncryptedBytes()).equals(Utils.bytesToHexString(other.key.getEncryptedPrivateKey().getEncryptedBytes()))) {
            // Note you can have two equivalent keys (with the same private key) and when encrypted and they appear different due to the random IV.
            return false;
        }
        return true;
    }

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
