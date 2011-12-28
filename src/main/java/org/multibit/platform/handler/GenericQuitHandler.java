package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitResponse;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericQuitHandler extends GenericHandler {
    /**
     * Called in response to receiving a Quit event
     * @param event The generic Quit event
     * @param response The response containing the methods to call if quit can continue or not
     */
    void handleQuitRequestWith(GenericQuitEvent event, GenericQuitResponse response);
}
