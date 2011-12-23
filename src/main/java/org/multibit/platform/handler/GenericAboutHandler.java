package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericAboutEvent;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericAboutHandler extends GenericHandler {
    /**
     * Called in response to receiving an About event
     * @param event The generic About event
     */
    void handleAbout(GenericAboutEvent event);
}
