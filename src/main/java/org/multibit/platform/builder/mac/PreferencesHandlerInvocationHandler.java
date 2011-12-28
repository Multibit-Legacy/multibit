package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericPreferencesHandler;
import org.multibit.platform.listener.GenericPreferencesEvent;

/**
 * <p>Implementation of invocation handler for Preferences</p>
 */
public class PreferencesHandlerInvocationHandler extends BaseMacInvocationHandler<GenericPreferencesHandler, GenericPreferencesEvent> {

    PreferencesHandlerInvocationHandler(GenericPreferencesHandler genericHandler, Class<GenericPreferencesEvent> genericEventClass) {
        super(genericHandler, genericEventClass);
    }
}
