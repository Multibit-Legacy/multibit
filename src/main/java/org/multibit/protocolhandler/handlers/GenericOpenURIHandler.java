package org.multibit.protocolhandler.handlers;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.protocolhandler.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericOpenURIHandler {
    /**
     * Called in response to receiving an open URI event
     * @param event The generic open URI event
     */
    void openURI(GenericOpenURIEvent event);
}
