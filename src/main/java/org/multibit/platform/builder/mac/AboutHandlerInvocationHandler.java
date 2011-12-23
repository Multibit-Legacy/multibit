package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericAboutHandler;
import org.multibit.platform.listener.GenericAboutEvent;

/**
 * <p>Implementation of invocation handler for About event</p>
 */
public class AboutHandlerInvocationHandler extends BaseMacInvocationHandler<GenericAboutHandler, GenericAboutEvent> {

    AboutHandlerInvocationHandler(GenericAboutHandler genericHandler, Class<GenericAboutEvent> genericEventClass) {
        super(genericHandler, genericEventClass);
    }
}
