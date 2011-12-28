package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericQuitHandler;
import org.multibit.platform.listener.GenericQuitResponse;

/**
 * <p>Implementation of invocation handler for Quit response</p>
 */
public class QuitResponseInvocationHandler extends BaseMacInvocationHandler<GenericQuitHandler, GenericQuitResponse> {

    QuitResponseInvocationHandler(GenericQuitHandler genericHandler, Class<GenericQuitResponse> genericEventClass) {
        super(genericHandler, genericEventClass);
    }
}
