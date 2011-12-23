package org.multibit.platform.listener;

/**
 * <p>Listener to provide the following to applications:</p>
 * <ul>
 * <li>Notification of a preferences event</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericPreferencesEventListener {
    /**
     * Received when the user requests the Preferences display
     *
     * @param event The event
     */
    void onPreferencesEvent(GenericPreferencesEvent event);
}
