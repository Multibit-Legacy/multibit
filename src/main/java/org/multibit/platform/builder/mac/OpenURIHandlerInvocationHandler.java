package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericOpenURIHandler;
import org.multibit.platform.listener.GenericOpenURIEvent;

/**
 * <p>Implementation of invocation handler for OpenURI</p>
 */
public class OpenURIHandlerInvocationHandler extends BaseMacInvocationHandler<GenericOpenURIHandler, GenericOpenURIEvent> {

    OpenURIHandlerInvocationHandler(GenericOpenURIHandler genericHandler, Class<GenericOpenURIEvent> genericEventClass) {
        super(genericHandler, genericEventClass);
    }
}
