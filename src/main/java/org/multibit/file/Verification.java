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

/**
 * A value object used by PrivateKeysHandler to report on whether an import file is verified correctly
 * @author jim
 *
 */
public class Verification {
    
    private boolean isCorrect;
    
    private String messageKey;
    
    private Object[] messageData;

    public Verification(boolean isCorrect, String messageKey, Object[] messageData) {
        super();
        this.isCorrect = isCorrect;
        this.messageKey = messageKey;
        this.messageData = messageData;
    }

    public boolean isCorrect() {
        return isCorrect;
    }

    public void setCorrect(boolean isCorrect) {
        this.isCorrect = isCorrect;
    }

    public String getMessageKey() {
        return messageKey;
    }

    public void setMessageKey(String messageKey) {
        this.messageKey = messageKey;
    }

    public Object[] getMessageData() {
        return messageData;
    }

    public void setMessageData(Object[] messageData) {
        this.messageData = messageData;
    }
    

}
