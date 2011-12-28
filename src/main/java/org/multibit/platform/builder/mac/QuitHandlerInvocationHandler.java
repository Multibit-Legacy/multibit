package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericQuitHandler;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitResponse;

/**
 * <p>Implementation of invocation handler for Quit</p>
 */
public class QuitHandlerInvocationHandler extends BaseMacResponseInvocationHandler<GenericQuitHandler, GenericQuitEvent, GenericQuitResponse> {

    QuitHandlerInvocationHandler(GenericQuitHandler genericHandler, Class<GenericQuitEvent> genericEventClass, Class<GenericQuitResponse> genericResponseClass) {
        super(genericHandler, genericEventClass, genericResponseClass);
    }
}
