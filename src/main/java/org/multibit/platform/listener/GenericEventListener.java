package org.multibit.platform.listener;

import java.util.Collection;

/**
 * <p>Generic interface to provide the following to generic event handlers:</p>
 * <ul>
 * <li>Type safety for adding and removing listeners</li>
 * </ul>
 * <p>See {@link org.multibit.platform.handler.DefaultOpenURIHandler} for an example of an implementation</p>
 *
 * @since 0.3.0
 *         
 */
public interface GenericEventListener<T> {
    /**
     * @param listeners The listeners to add to the handler
     */
    void addListeners(Collection<T> listeners);
}
