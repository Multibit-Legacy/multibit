package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericEventListener;
import org.multibit.platform.listener.GenericOpenURIEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * <p>Generic event to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific event handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class DefaultOpenURIHandler implements GenericOpenURIHandler, GenericEventListener<GenericOpenURIEventListener> {
    private static final Logger log = LoggerFactory.getLogger(DefaultOpenURIHandler.class);

    // The event listeners
    private Set<GenericOpenURIEventListener> listeners = new HashSet<GenericOpenURIEventListener>();

    /**
     * Handles the process of broadcasting the event to listeners
     * allowing this process to be decoupled
     * @param event The generic event (or it's proxy)
     */
    @Override
    public void openURI(GenericOpenURIEvent event) {
        log.debug("Called");
        if (event == null) {
            log.warn("Received a null event");
            return;
        }
        log.debug("Event class is {}",event.getClass().getSimpleName());
        log.debug("Received open URI request of '{}'",event.getURI());
        log.debug("Broadcasting to {} listener(s)",listeners.size());
        for (GenericOpenURIEventListener listener: listeners) {
            listener.onOpenURIEvent(event);
        }
    }

    @Override
    public void addListeners(Collection<GenericOpenURIEventListener> listeners) {
        this.listeners.addAll(listeners);
    }

}
