package org.multibit.platform.handler;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericPreferencesHandler {
    /**
     * Called in response to receiving a preferences event
     * @param event The generic preferences event
     */
    void handlePreferences(GenericPreferencesEvent event);
}
