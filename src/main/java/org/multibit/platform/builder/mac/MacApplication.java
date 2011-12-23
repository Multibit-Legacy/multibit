package org.multibit.platform.builder.mac;

import org.multibit.platform.GenericApplication;
import org.multibit.platform.handler.GenericOpenURIHandler;
import org.multibit.platform.handler.GenericPreferencesHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;

/**
 * <p>GenericApplication to provide the following to application:</p>
 * <ul>
 * <li>Provision of Apple Mac specific implementations of common methods</li>
 * </ul>
 * <p>TODO Fill in more support code as and when required using the EAWT Javadocs as a reference for non-deprecated methods</p>
 * @see <a href="http://developer.apple.com/library/mac/documentation/Java/Reference/JavaSE6_AppleExtensionsRef/api/index.html?com/apple/eawt/Application.html">The Apple EAWT Javadocs</a>

 *
 * @since 0.3.0
 *         
 */
public class MacApplication implements GenericApplication {

    private static final Logger log = LoggerFactory.getLogger(MacApplication.class);

    /**
     * The native EAWT Application instance providing OS events
     */
    private Object nativeApplication;

    /**
     * Handles the OpenURI use case
     */
    private Class nativeOpenURIHandlerClass;
    /**
     * Handles the Preferences use case
     */
    private Class nativePreferencesHandlerClass;

    public void addOpenURIHandler(GenericOpenURIHandler openURIHandler) {

        log.debug("Adding GenericOpenURIHandler");
        // Ensure the implementing class is public
        // This avoids anonymous interface issues
        if (!Modifier.isPublic(openURIHandler.getClass().getModifiers())) {
            throw new IllegalArgumentException("GenericOpenURIHandler must be a public class");
        }

        // Load up an instance of the native OpenURIHandler
        // Provide an invocation handler to link the native openURI(AppEvent.OpenURIEvent event)
        // back to the generic handler
        Object nativeOpenURIHandler = Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class[]{nativeOpenURIHandlerClass},
            new OpenURIHandlerInvocationHandler(openURIHandler));

        // Reflective call as application.setOpenURIHandler(nativeOpenURIHandler)
        // nativeOpenURIHandler is a proxy that actually uses the generic handler
        callNativeMethod(nativeApplication, "setOpenURIHandler", new Class[]{nativeOpenURIHandlerClass}, new Object[]{nativeOpenURIHandler});

        log.debug("GenericOpenURIHandler configured");

    }

    public void addPreferencesHandler(GenericPreferencesHandler preferencesHandler) {

        log.debug("Adding GenericPreferencesHandler");
        // Ensure the implementing class is public
        // This avoids anonymous interface issues
        if (!Modifier.isPublic(preferencesHandler.getClass().getModifiers())) {
            throw new IllegalArgumentException("GenericPreferencesHandler must be a public class");
        }

        // Load up an instance of the native PreferencesHandler
        // Provide an invocation handler to link the native preferences(AppEvent.PreferencesEvent event)
        // back to the generic handler
        Object nativePreferencesHandler = Proxy.newProxyInstance(getClass().getClassLoader(),
            new Class[]{nativePreferencesHandlerClass},
            new PreferencesHandlerInvocationHandler(preferencesHandler));

        // Reflective call as application.setPreferencesHandler(nativePreferencesHandler)
        // nativePreferencesHandler is a proxy that actually uses the generic handler
        callNativeMethod(nativeApplication, "setPreferencesHandler", new Class[]{nativePreferencesHandlerClass}, new Object[]{nativePreferencesHandler});

        log.debug("GenericPreferencesHandler configured");

    }

    /**
     * Calls a non-zero argument method of the given (usually native) object
     * @param object The object
     * @param methodName The method name
     * @param classes The classes of the arguments in the order they appear in the method signature
     * @param arguments The values of the arguments in the order they appear in the method signature
     * @return The result of the call
     */
    private Object callNativeMethod(Object object, String methodName, Class[] classes, Object[] arguments) {
        log.debug("Calling methodName {}", methodName);
        try {
            // Build a suitable Class[] for the method signature based on the arguments
            if (classes == null) {
                classes = new Class[arguments.length];
                for (int i = 0; i < classes.length; i++) {
                    classes[i] = arguments[i].getClass();

                }
            }
            Method method = object.getClass().getMethod(methodName, classes);
            return method.invoke(object, arguments);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean isMac() {
        return true;
    }

    @Override
    public boolean isLinux() {
        return false;
    }

    @Override
    public boolean isWindows() {
        return false;
    }

    public void setApplication(Object application) {
        this.nativeApplication = application;
    }

    public void setOpenURIHandlerClass(Class openURIHandlerClass) {
        this.nativeOpenURIHandlerClass = openURIHandlerClass;
    }

    public void setPreferencesHandlerClass(Class preferencesHandlerClass) {
        this.nativePreferencesHandlerClass = preferencesHandlerClass;
    }

}


