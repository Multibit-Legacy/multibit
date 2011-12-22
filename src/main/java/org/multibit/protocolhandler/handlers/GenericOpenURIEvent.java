package org.multibit.protocolhandler.handlers;

import java.net.URI;

/**
 * <p>Generic event to provide the following to {@link org.multibit.protocolhandler.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific event handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericOpenURIEvent {

    /**
     * @return The URI that should be opened
     */
    URI getURI();
}
