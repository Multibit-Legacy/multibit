package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericOpenURIEvent;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Proxies any native handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericOpenURIHandler extends GenericHandler {
    /**
     * Called in response to receiving an open URI event
     * @param event The generic open URI event
     */
    void openURI(GenericOpenURIEvent event);
}
