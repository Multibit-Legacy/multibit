package org.multibit.platform.listener;

import org.multibit.platform.handler.GenericPreferencesEvent;

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
    void onPreferencesEvent(GenericPreferencesEvent event);
}
