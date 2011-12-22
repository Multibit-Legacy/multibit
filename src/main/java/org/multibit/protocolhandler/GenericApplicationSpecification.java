package org.multibit.protocolhandler;

import java.util.HashSet;
import java.util.Set;

/**
 * <p>Specification value object to provide the following to {@link GenericApplicationFactory}:</p>
 * <ul>
 * <li>Provision of required references to external objects</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class GenericApplicationSpecification {

    private Set<GenericOpenURIEventListener> openURIEventListeners= new HashSet<GenericOpenURIEventListener>();

    public Set<GenericOpenURIEventListener> getOpenURIEventListeners() {
        return openURIEventListeners;
    }

}
