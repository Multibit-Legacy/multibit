package org.multibit.platform.listener;

import org.multibit.platform.handler.GenericOpenURIEvent;

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
    void onOpenURIEvent(GenericOpenURIEvent event);
}
