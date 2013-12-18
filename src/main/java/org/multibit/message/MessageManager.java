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
package org.multibit.message;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * Enum singleton to manage MultiBit (and in future system wide) messages.
 * @author jim
 */
public enum MessageManager {
    INSTANCE;

    public static final int MAXIMUM_NUMBER_OF_MESSAGES_STORED = 512;
    
    /**
     * Queue containing Messages received.
     */
    private LinkedBlockingQueue<Message> messageQueue;
    
    /**
     * Message listeners.
     */
    private Collection<MessageListener> messageListeners;
    
    MessageManager() {
        messageQueue = new LinkedBlockingQueue<Message>(MAXIMUM_NUMBER_OF_MESSAGES_STORED);
        messageListeners = new ArrayList<MessageListener>();
    }
    
    synchronized public void addMessage(Message message) {
        if (message != null) {
            boolean wasAdded = messageQueue.offer(message);
            if (!wasAdded) {
                // remove the oldest message
                messageQueue.poll();
                
                // add it again
                messageQueue.offer(message);
            }
            notifyMessageListeners(message);
        }
    }
    
    public void addMessageListener(MessageListener messageListener) {
        messageListeners.add(messageListener);
    }
    
    private void notifyMessageListeners(Message message) {
        for (MessageListener listener : messageListeners) {
            if (listener != null) {
                listener.newMessageReceived(message);
            }
        }
    }
    
    public Collection<Message> getMessages() {
        return Collections.unmodifiableCollection(messageQueue);
    }   
}
