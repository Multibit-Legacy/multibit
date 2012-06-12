package org.multibit.message;

/**
 * An interface for receiving message notifications.
 * @author jim
 *
 */
public interface MessageListener {

    /**
     * Method indicating a new message has been received
     */
    public void newMessageReceived(Message newMessage);
}
