package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericOpenURIEvent;
import org.multibit.platform.handler.GenericOpenURIHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>Adapter between the native system call and the generic implementation</p>
 * <p>In this case, the native system attempts to invoke a method </p>
 * Handles the reflective invocation of the {@link org.multibit.platform.handler.GenericOpenURIHandler} from within the
 */
class OpenURIHandlerInvocationHandler implements InvocationHandler {

    private static final Logger log = LoggerFactory.getLogger(OpenURIHandlerInvocationHandler.class);

    private GenericOpenURIHandler openURIHandler;

    OpenURIHandlerInvocationHandler(GenericOpenURIHandler openURIHandler) {
        this.openURIHandler = openURIHandler;
    }

    /**
     * Handles the invocation by the native system against the generic event handler
     * @param object The object
     * @param nativeMethod The native method
     * @param objects The arguments
     *
     * @return The result of the call
     *
     * @throws Throwable
     */
    public Object invoke(Object object, Method nativeMethod, Object[] objects) throws Throwable {

        log.debug("Invoked nativeMethod={}, method args length={}", nativeMethod.getName(), objects.length);
        // Create a generic event using the object parameters
        GenericOpenURIEvent event = createGenericURIEvent(objects[0]);
        try {
            log.debug("Created GenericOpenURIEvent");

            // Access the equivalent method on the generic handler (e.g. openURI(GenericOpenURIEvent event))
            Method method = openURIHandler.getClass().getMethod(nativeMethod.getName(), new Class[]{GenericOpenURIEvent.class});

            // Invoke the method passing in the event (e.g. GenericURIEvent)
            log.debug("Invoking {}.{}({}) ", new Object[]{openURIHandler.getClass().getSimpleName(), method.getName(), method.getParameterTypes()[0].getSimpleName()});
            return method.invoke(openURIHandler, event);
        } catch (NoSuchMethodException e) {
            log.warn("Got a NoSuchMethodException");
            if (nativeMethod.getName().equals("equals") && objects.length == 1) {
                return object == objects[0];
            }
            return null;
        }
    }

    /**
     * <p>Create a proxy instance of {@link org.multibit.platform.handler.GenericOpenURIEvent}
     * that delegates to the native Apple OpenURIEvent</p>
     *
     * @param nativeOpenURIEvent The Apple native OpenURIEvent
     *
     * @return The generic URI event acting as a proxy
     */
    private GenericOpenURIEvent createGenericURIEvent(final Object nativeOpenURIEvent) {
        log.debug("Building invocation handler against object {}", nativeOpenURIEvent);
        // The invocation handler manages all method calls against the proxy
        // Relies on the proxy having the same method signatures
        InvocationHandler invocationHandler = new InvocationHandler() {
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                log.debug("GenericOpenURIEvent called");
                Method nativeMethod = nativeOpenURIEvent.getClass().getMethod(method.getName(), method.getParameterTypes());
                log.debug("Invoking method {}.{}", nativeOpenURIEvent.getClass().getSimpleName(), method.getName());
                return nativeMethod.invoke(nativeOpenURIEvent, objects);
            }
        };

        log.debug("Building proxy for GenericOpenURIEvent");
        // Create a proxy that delegates the call to the native instance
        // when the invocation handler is called against it's methods
        return (GenericOpenURIEvent) Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class[]{GenericOpenURIEvent.class}, invocationHandler);
    }

}
