package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericPreferencesEvent;
import org.multibit.platform.handler.GenericPreferencesHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * Handles the reflective invocation of the {@link org.multibit.platform.handler.GenericPreferencesHandler} from within the
 */
class PreferencesHandlerInvocationHandler implements InvocationHandler {

    private static final Logger log = LoggerFactory.getLogger(PreferencesHandlerInvocationHandler.class);

    private GenericPreferencesHandler preferenceshandler;

    PreferencesHandlerInvocationHandler(GenericPreferencesHandler PreferencesHandler) {
        this.preferenceshandler = PreferencesHandler;
    }

    /**
     * Handles the invocation
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
        GenericPreferencesEvent event = createGenericURIEvent(objects[0]);
        try {
            log.debug("Created GenericPreferencesEvent");

            // Access the equivalent method on the generic handler (e.g. Preferences(GenericPreferencesEvent event))
            Method method = preferenceshandler.getClass().getMethod(nativeMethod.getName(), new Class[]{GenericPreferencesEvent.class});

            // Invoke the method passing in the event (e.g. GenericURIEvent)
            log.debug("Invoking {}.{}({}) ", new Object[]{preferenceshandler.getClass().getSimpleName(), method.getName(), method.getParameterTypes()[0].getSimpleName()});
            return method.invoke(preferenceshandler, event);
        } catch (NoSuchMethodException e) {
            log.warn("Got a NoSuchMethodException");
            if (nativeMethod.getName().equals("equals") && objects.length == 1) {
                return object == objects[0];
            }
            return null;
        }
    }

    /**
     * <p>Create a proxy instance of {@link org.multibit.platform.handler.GenericPreferencesEvent}
     * that delegates to the native Apple PreferencesEvent</p>
     *
     * @param nativePreferencesEvent The Apple native PreferencesEvent
     *
     * @return The generic URI event acting as a proxy
     */
    private GenericPreferencesEvent createGenericURIEvent(final Object nativePreferencesEvent) {
        log.debug("Building invocation handler against object {}", nativePreferencesEvent);
        // The invocation handler manages all method calls against the proxy
        // Relies on the proxy having the same method signatures
        InvocationHandler invocationHandler = new InvocationHandler() {
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                log.debug("GenericPreferencesEvent called");
                Method nativeMethod = nativePreferencesEvent.getClass().getMethod(method.getName(), method.getParameterTypes());
                log.debug("Invoking method {}.{}", nativePreferencesEvent.getClass().getSimpleName(), method.getName());
                return nativeMethod.invoke(nativePreferencesEvent, objects);
            }
        };

        log.debug("Building proxy for GenericPreferencesEvent");
        // Create a proxy that delegates the call to the native instance
        // when the invocation handler is called against it's methods
        return (GenericPreferencesEvent) Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class[]{GenericPreferencesEvent.class}, invocationHandler);
    }

}
