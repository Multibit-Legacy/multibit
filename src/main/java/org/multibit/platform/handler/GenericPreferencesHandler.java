package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericPreferencesEvent;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericPreferencesHandler extends GenericHandler {
    /**
     * Called in response to receiving a Preferences event
     * @param event The generic Preferences event
     */
    void handlePreferences(GenericPreferencesEvent event);
}
