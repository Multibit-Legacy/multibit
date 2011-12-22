package org.multibit.protocolhandler.builders;

import org.multibit.protocolhandler.GenericApplication;
import org.multibit.protocolhandler.handlers.GenericOpenURIHandler;
import org.multibit.protocolhandler.handlers.GenericOpenURIEvent;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>GenericApplication to provide the following to application:</p>
 * <ul>
 * <li>Provision of Apple Mac specific implementations of common methods</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class MacApplication implements GenericApplication {

    private static final Logger log = LoggerFactory.getLogger(MacApplication.class);

    private Object nativeApplication;
    // TODO Remove this
    private Class nativeApplicationListenerClass;
    private Class nativeOpenURIHandlerClass;

    /**
     * The native application object
     */

    Map handlerMap = Collections.synchronizedMap(new HashMap());
    private boolean enabledAboutMenu = true;
    private boolean enabledPreferencesMenu;
    private boolean aboutMenuItemPresent = true;
    private boolean preferencesMenuItemPresent;
    private ClassLoader classLoader;

    public void addAboutMenuItem() {
        if (isMac()) {
            callMethod(nativeApplication, "addAboutMenuItem");
        } else {
            this.aboutMenuItemPresent = true;
        }
    }

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
        callMethod(nativeApplication, "setOpenURIHandler", new Class[]{nativeOpenURIHandlerClass}, new Object[]{nativeOpenURIHandler});

        log.debug("GenericOpenURIHandler configured");

        // Keep track of this handler
        handlerMap.put(openURIHandler, nativeOpenURIHandler);

    }

    @Deprecated
    public void addApplicationListener(ApplicationListener applicationListener) {

        if (!Modifier.isPublic(applicationListener.getClass().getModifiers())) {
            throw new IllegalArgumentException("ApplicationListener must be a public class");
        }
        if (isMac()) {
            Object listener = Proxy.newProxyInstance(getClass().getClassLoader(),
                    new Class[]{nativeApplicationListenerClass},
                    new ApplicationListenerInvocationHandler(applicationListener));

            callMethod(nativeApplication, "addApplicationListener", new Class[]{nativeApplicationListenerClass}, new Object[]{listener});
            handlerMap.put(applicationListener, listener);
        } else {
            handlerMap.put(applicationListener, applicationListener);
        }
    }

    @Deprecated
    public void addPreferencesMenuItem() {
        if (isMac()) {
            callMethod("addPreferencesMenuItem");
        } else {
            this.preferencesMenuItemPresent = true;
        }
    }

    public boolean getEnabledAboutMenu() {
        if (isMac()) {
            return callMethod("getEnabledAboutMenu").equals(Boolean.TRUE);
        } else {
            return enabledAboutMenu;
        }
    }

    public boolean getEnabledPreferencesMenu() {
        if (isMac()) {
            Object result = callMethod("getEnabledPreferencesMenu");
            return result.equals(Boolean.TRUE);
        } else {
            return enabledPreferencesMenu;
        }
    }

    public Point getMouseLocationOnScreen() {
        if (isMac()) {
            try {
                Method method = nativeApplication.getClass().getMethod("getMouseLocationOnScreen", new Class[0]);
                return (Point) method.invoke(null);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException(e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        } else {
            return new Point(0, 0);
        }
    }

    public boolean isAboutMenuItemPresent() {
        if (isMac()) {
            return callMethod("isAboutMenuItemPresent").equals(Boolean.TRUE);
        } else {
            return aboutMenuItemPresent;
        }
    }

    public boolean isPreferencesMenuItemPresent() {
        if (isMac()) {
            return callMethod("isPreferencesMenuItemPresent").equals(Boolean.TRUE);
        } else {
            return this.preferencesMenuItemPresent;
        }
    }

    public void removeAboutMenuItem() {
        if (isMac()) {
            callMethod("removeAboutMenuItem");
        } else {
            this.aboutMenuItemPresent = false;
        }
    }

    public synchronized void removeApplicationListener(ApplicationListener applicationListener) {
        if (isMac()) {
            Object listener = handlerMap.get(applicationListener);
            callMethod(nativeApplication, "removeApplicationListener", new Class[]{nativeApplicationListenerClass}, new Object[]{listener});

        }
        handlerMap.remove(applicationListener);
    }

    public void removePreferencesMenuItem() {
        if (isMac()) {
            callMethod("removeAboutMenuItem");
        } else {
            this.preferencesMenuItemPresent = false;
        }
    }

    public void setEnabledAboutMenu(boolean enabled) {
        if (isMac()) {
            callMethod(nativeApplication, "setEnabledAboutMenu", new Class[]{Boolean.TYPE}, new Object[]{Boolean.valueOf(enabled)});
        } else {
            this.enabledAboutMenu = enabled;
        }
    }

    public void setEnabledPreferencesMenu(boolean enabled) {
        if (isMac()) {
            callMethod(nativeApplication, "setEnabledPreferencesMenu", new Class[]{Boolean.TYPE}, new Object[]{Boolean.valueOf(enabled)});
        } else {
            this.enabledPreferencesMenu = enabled;
        }

    }

    public int requestUserAttention(int type) {
        return 0;
    }

    public void cancelUserAttentionRequest(int request) {
        try {
            Object application = getNSApplication();
            application.getClass().getMethod("cancelUserAttentionRequest", new Class[]{Integer.TYPE}).invoke(application, new Object[]{new Integer(request)});
        } catch (ClassNotFoundException e) {
            // Nada
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    private Object getNSApplication() throws ClassNotFoundException {
        try {
            Class applicationClass = Class.forName("com.apple.cocoa.application.NSApplication");
            return applicationClass.getMethod("sharedApplication", new Class[0]).invoke(null, new Object[0]);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    public void setApplicationIconImage(BufferedImage image) {
        if (isMac()) {
            try {
                Method setDockIconImage = nativeApplication.getClass().getMethod("setDockIconImage", Image.class);

                try {
                    setDockIconImage.invoke(nativeApplication, image);
                } catch (IllegalAccessException e) {

                } catch (InvocationTargetException e) {

                }
            } catch (NoSuchMethodException mnfe) {


                ByteArrayOutputStream stream = new ByteArrayOutputStream();
                try {
                    ImageIO.write(image, "png", stream);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }

                try {
                    Class nsDataClass = Class.forName("com.apple.cocoa.foundation.NSData");
                    Constructor constructor = nsDataClass.getConstructor(new Class[]{new byte[0].getClass()});

                    Object nsData = constructor.newInstance(new Object[]{stream.toByteArray()});

                    Class nsImageClass = Class.forName("com.apple.cocoa.application.NSImage");
                    Object nsImage = nsImageClass.getConstructor(new Class[]{nsDataClass}).newInstance(new Object[]{nsData});

                    Object application = getNSApplication();

                    application.getClass().getMethod("setApplicationIconImage", new Class[]{nsImageClass}).invoke(application, new Object[]{nsImage});

                } catch (ClassNotFoundException e) {

                } catch (NoSuchMethodException e) {
                    throw new RuntimeException(e);
                } catch (IllegalAccessException e) {
                    throw new RuntimeException(e);
                } catch (InvocationTargetException e) {
                    throw new RuntimeException(e);
                } catch (InstantiationException e) {
                    throw new RuntimeException(e);
                }

            }

        }
    }

    public BufferedImage getApplicationIconImage() {
        if (isMac()) {

            try {
                Method getDockIconImage = nativeApplication.getClass().getMethod("getDockIconImage");
                try {
                    return (BufferedImage) getDockIconImage.invoke(nativeApplication);
                } catch (IllegalAccessException e) {

                } catch (InvocationTargetException e) {

                }
            } catch (NoSuchMethodException nsme) {

                try {
                    Class nsDataClass = Class.forName("com.apple.cocoa.foundation.NSData");
                    Class nsImageClass = Class.forName("com.apple.cocoa.application.NSImage");
                    Object application = getNSApplication();
                    Object nsImage = application.getClass().getMethod("applicationIconImage", new Class[0]).invoke(application, new Object[0]);

                    Object nsData = nsImageClass.getMethod("TIFFRepresentation", new Class[0]).invoke(nsImage, new Object[0]);

                    Integer length = (Integer) nsDataClass.getMethod("length", new Class[0]).invoke(nsData, new Object[0]);
                    byte[] bytes = (byte[]) nsDataClass.getMethod("bytes", new Class[]{Integer.TYPE, Integer.TYPE}).invoke(nsData, new Object[]{Integer.valueOf(0), length});

                    return ImageIO.read(new ByteArrayInputStream(bytes));

                } catch (ClassNotFoundException e) {
                    e.printStackTrace();
                } catch (NoSuchMethodException e) {
                    throw new RuntimeException(e);
                } catch (IllegalAccessException e) {
                    throw new RuntimeException(e);
                } catch (InvocationTargetException e) {
                    throw new RuntimeException(e);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }

        }

        return null;
    }

    private Object callMethod(String methodName) {
        return callMethod(nativeApplication, methodName, new Class[0], new Object[0]);
    }

    private Object callMethod(Object object, String methodName) {
        return callMethod(object, methodName, new Class[0], new Object[0]);
    }

    private Object callMethod(Object object, String methodName, Class[] classes, Object[] arguments) {
        log.debug("Calling methodName {}",methodName);
        try {
            // Build a suitable Class[] for the method signature based on the arguments
            if (classes == null) {
                classes = new Class[arguments.length];
                for (int i = 0; i < classes.length; i++) {
                    classes[i] = arguments[i].getClass();

                }
            }
            Method addListenerMethod = object.getClass().getMethod(methodName, classes);
            return addListenerMethod.invoke(object, arguments);
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
}

/**
 * Handles the reflective invocation of the {@link GenericOpenURIHandler} from within the
 */
class OpenURIHandlerInvocationHandler implements InvocationHandler {

    private static final Logger log = LoggerFactory.getLogger(OpenURIHandlerInvocationHandler.class);

    private GenericOpenURIHandler openURIHandler;

    OpenURIHandlerInvocationHandler(GenericOpenURIHandler openURIHandler) {
        this.openURIHandler = openURIHandler;
    }

    /**
     * @param object
     * @param nativeMethod
     * @param objects
     * @return
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
            log.debug("Invoking {}.{}({}) ",new Object[] {openURIHandler.getClass().getSimpleName(), method.getName(), method.getParameterTypes()[0].getSimpleName()});
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
     * <p>Create a proxy instance of {@link org.multibit.protocolhandler.handlers.GenericOpenURIEvent}
     * that delegates to the native Apple OpenURIEvent</p>
     * @param nativeOpenURIEvent The Apple native OpenURIEvent
     * @return The generic URI event acting as a proxy
     */
    private GenericOpenURIEvent createGenericURIEvent(final Object nativeOpenURIEvent) {
        log.debug("Building invocation handler against object {}",nativeOpenURIEvent);
        // The invocation handler manages all method calls against the proxy
        // Relies on the proxy having the same method signatures
        InvocationHandler invocationHandler = new InvocationHandler() {
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                log.debug("GenericOpenURIEvent called");
                Method nativeMethod=nativeOpenURIEvent.getClass().getMethod(method.getName(), method.getParameterTypes());
                log.debug("Invoking method {}.{}",nativeOpenURIEvent.getClass().getSimpleName(),method.getName());
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


class ApplicationListenerInvocationHandler implements InvocationHandler {
    private ApplicationListener applicationListener;

    ApplicationListenerInvocationHandler(ApplicationListener applicationListener) {
        this.applicationListener = applicationListener;
    }

    public Object invoke(Object object, Method appleMethod, Object[] objects) throws Throwable {

        ApplicationEvent event = createApplicationEvent(objects[0]);
        try {
            Method method = applicationListener.getClass().getMethod(appleMethod.getName(), new Class[]{ApplicationEvent.class});
            return method.invoke(applicationListener, new Object[]{event});
        } catch (NoSuchMethodException e) {
            if (appleMethod.getName().equals("equals") && objects.length == 1) {
                return Boolean.valueOf(object == objects[0]);
            }
            return null;
        }
    }


    private ApplicationEvent createApplicationEvent(final Object appleApplicationEvent) {
        return (ApplicationEvent) Proxy.newProxyInstance(getClass().getClassLoader(), new Class[]{ApplicationEvent.class}, new InvocationHandler() {
            public Object invoke(Object o, Method method, Object[] objects) throws Throwable {
                return appleApplicationEvent.getClass().getMethod(method.getName(), method.getParameterTypes()).invoke(appleApplicationEvent, objects);
            }
        });
    }

}
