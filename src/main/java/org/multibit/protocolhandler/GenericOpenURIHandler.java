package org.multibit.protocolhandler;

/**
 * <p>Generic handler to provide the following to {@link GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericOpenURIHandler {
    void openURI(GenericURIEvent event);
}
