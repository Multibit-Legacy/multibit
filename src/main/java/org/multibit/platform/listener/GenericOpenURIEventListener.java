package org.multibit.platform.listener;

/**
 * <p>Listener to provide the following to applications:</p>
 * <ul>
 * <li>Notification of an Open URI event</li>
 * </ul>
 * <p>Example:</p>
 * <pre>
 * </pre>
 *
 * @since 0.3.0
 *         
 */
public interface GenericOpenURIEventListener {
    /**
     * Received when the application receives an open URI event
     *
     * @param event The event
     */
    void onOpenURIEvent(GenericOpenURIEvent event);
}
