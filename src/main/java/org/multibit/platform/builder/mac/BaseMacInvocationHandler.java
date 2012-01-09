package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericHandler;
import org.multibit.platform.listener.GenericEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * <p>Base class to provide the following to Mac invocation handlers:</p>
 * <ul>
 * <li>Provision of standard template handling for simple events (no responses)</li>
 * </ul>
 * <p>Template keys are:</p>
 * <ul>
 * <li><code>H</code>: The generic handler class (e.g. {@link AboutHandlerInvocationHandler}</li>
 * <li><code>E</code>: The generic event class (e.g. {@link org.multibit.platform.listener.GenericAboutEvent}</li>
 * </ul>
 *
 * @since 0.3.0
 */
public abstract class BaseMacInvocationHandler<H extends GenericHandler, E extends GenericEvent> implements InvocationHandler {
    private static final Logger log = LoggerFactory.getLogger(BaseMacInvocationHandler.class);

    private final H genericHandler;
    private final Class<E> genericEventClass;

    BaseMacInvocationHandler(H genericHandler, Class<E> genericEventClass) {
        this.genericHandler = genericHandler;
        this.genericEventClass = genericEventClass;
    }

    /**
     * Handles the invocation
     *
     * @param object       The object
     * @param nativeMethod The native method
     * @param objects      The arguments
     *
     * @return The result of the call
     *
     * @throws Throwable If something goes wrong
     */
    @Override
    @SuppressWarnings("unchecked")
    public Object invoke(Object object, Method nativeMethod, Object[] objects) throws Throwable {

        log.debug("Invoked. NativeMethod={}, method args length={}", nativeMethod.getName(), objects.length);

        // Create a uni-directional generic event based on a single parameter (the native event)
        // Require an unchecked cast here to avoid this issue:
        // http://blog.sarathonline.com/2010/08/maven-only-type-parameters-of-x-cannot.html
        E event = (E) createGenericEvent(objects[0]);
        try {
            log.debug("Created event {}", genericEventClass.getSimpleName());

            // Access the equivalent method on the generic handler (e.g. handleQuit(GenericQuitEvent event))
            Method method = genericHandler.getClass().getMethod(nativeMethod.getName(), new Class[]{genericEventClass});

            // Invoke the method passing in the event (e.g. GenericURIEvent)
            log.debug("Invoking {}.{}({}) ", new Object[]{genericHandler.getClass().getSimpleName(), method.getName(), method.getParameterTypes()[0].getSimpleName()});
            return method.invoke(genericHandler, event);
        } catch (NoSuchMethodException e) {
            log.warn("Got a NoSuchMethodException");
            if (nativeMethod.getName().equals("equals") && objects.length == 1) {
                return object == objects[0];
            }
            return null;
        }
    }

    /**
     * <p>Create a proxy instance of {@link org.multibit.platform.listener.GenericEvent}
     * that delegates to the native Apple event</p>
     *
     * @param nativeEvent  The Apple native event (e.g. OpenURIEvent)
     *
     * @return The generic event acting as a proxy to the native underlying event
     */
    @SuppressWarnings("unchecked")
    private <E> E createGenericEvent(final Object nativeEvent) {
        log.debug("Building invocation handler. Native {} -> {}", nativeEvent.getClass().getSimpleName(), genericEventClass.getSimpleName());
        // The invocation handler manages all method calls against the proxy
        // Relies on the proxy having the same method signatures
        InvocationHandler invocationHandler = new InvocationHandler() {
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                Method nativeMethod = nativeEvent.getClass().getMethod(method.getName(), method.getParameterTypes());
                log.debug("Invoking method {}.{}", nativeEvent.getClass().getSimpleName(), method.getName());
                return nativeMethod.invoke(nativeEvent, objects);
            }
        };

        log.debug("Building proxy for generic event");

        // Create a proxy that delegates the call to the native instance
        // when the invocation handler is called against it's methods
        // Must use the application ClassLoader
        return (E) Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class[]{genericEventClass}, invocationHandler);
    }

}
