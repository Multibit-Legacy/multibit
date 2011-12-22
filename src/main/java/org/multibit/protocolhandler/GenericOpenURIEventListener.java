package org.multibit.protocolhandler;

import org.multibit.protocolhandler.handlers.GenericOpenURIEvent;

/**
 * <p>Listener to provide the following to applications:</p>
 * <ul>
 * <li>Notification of an Open URI event</li>
 * </ul>
 * <p>Example:</p>
 * <pre>
 * </pre>
 *
 * @since 1.0.0
 *         
 */
public interface GenericOpenURIEventListener {
    void onOpenURIEvent(GenericOpenURIEvent event);
}
