package org.multibit.platform;

import org.multibit.platform.listener.GenericOpenURIEventListener;
import org.multibit.platform.listener.GenericPreferencesEventListener;

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
    private Set<GenericPreferencesEventListener> preferencesEventListeners=new HashSet<GenericPreferencesEventListener>();

    public Set<GenericOpenURIEventListener> getOpenURIEventListeners() {
        return openURIEventListeners;
    }

    public Set<GenericPreferencesEventListener> getPreferencesEventListeners() {
        return preferencesEventListeners;
    }
}
