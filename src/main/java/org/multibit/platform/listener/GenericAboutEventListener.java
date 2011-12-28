package org.multibit.platform.listener;

/**
 * <p>Listener to provide the following to applications:</p>
 * <ul>
 * <li>Notification of an About dialog event</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericAboutEventListener {
    /**
     * Received when the user requests the About display
     *
     * @param event The event
     */
    void onAboutEvent(GenericAboutEvent event);
}
