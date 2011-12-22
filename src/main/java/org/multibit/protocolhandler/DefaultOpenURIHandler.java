package org.multibit.protocolhandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>Generic event to provide the following to {@link org.multibit.protocolhandler.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific event handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class DefaultOpenURIHandler implements GenericOpenURIHandler {
    private static final Logger log = LoggerFactory.getLogger(DefaultOpenURIHandler.class);

    @Override
    public void openURI(GenericURIEvent event) {
        log.warn("Wow! It bloody works!");
    }
}
