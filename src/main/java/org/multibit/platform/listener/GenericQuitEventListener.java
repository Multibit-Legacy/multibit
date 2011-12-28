package org.multibit.platform.listener;

/**
 * <p>Listener to provide the following to applications:</p>
 * <ul>
 * <li>Notification of a Quit event</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericQuitEventListener {
    /**
     * Received when the user requests to Quit
     *
     * @param event The event
     * @param response The response to prevent the quit operation if necessary
     */
    void onQuitEvent(GenericQuitEvent event, GenericQuitResponse response);
}
