package org.multibit.protocolhandler.builders;

import org.multibit.protocolhandler.DefaultOpenURIHandler;
import org.multibit.protocolhandler.GenericApplication;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * <p>[Pattern] to provide the following to {@link Object}:</p>
 * <ul>
 * <li></li>
 * </ul>
 * <p>Example:</p>
 * <pre>
 * </pre>
 *
 * @since 1.0.0
 *         
 */
public class MacApplicationBuilder {





    /**
     * <p>Builds either a {@link GenericApplication} if the platform does not provide EAWT support, or a {@link MacApplication} if it does.</p>
     * <p>Requires reflective code to avoid introducing Apple-specific code. The direct equivalent operation is as follows:</p>
     * <pre>
     *
     *  Application application = Application.getApplication();
     *  application.setAboutHandler(new AboutHandler() {
     *      void handleAbout(AppEvent.AboutEvent event) {
     *          // Fire the internal about handler event
     *      }
     *  });
     *  
     *  application.setOpenURIHandler(new OpenURIHandler() {
     *      void openFiles(AppEvent.OpenFilesEvent event) {
     *          // Fire the internal open file event
     *      }
     *  });
     *  
     *  application.setOpenFileHandler(new OpenFilesHandler() {
     *      void openFiles(AppEvent.OpenFilesEvent event) {
     *          // Fire the internal open file event
     *      }
     *  });
     * </pre>
     * @return A {@link GenericApplication}
     */
    public GenericApplication build() {

        Object nativeApplication;
        Class nativeApplicationListenerClass;
        Class nativeOpenURIHandlerClass;

        try {
            // Attempt to locate the Apple Java JDK
            final File file = new File("/System/Library/Java");
            if (file.exists()) {
                // Create a suitable class loader
                ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();
                Class systemClassLoaderClass = systemClassLoader.getClass();
                // Determine if class loading by URL is supported
                if (URLClassLoader.class.isAssignableFrom(systemClassLoaderClass)) {
                    // Get the addURL method from the class loader
                    Method addUrl = URLClassLoader.class.getDeclaredMethod("addURL", new Class[]{URL.class});
                    addUrl.setAccessible(true);
                    // Load the Apple JDK classes
                    addUrl.invoke(systemClassLoader, file.toURI().toURL());
                }
            }

            // Must have successfully loaded the Apple JDK or it's not there

            // Try to load the EAWT support classes
            Class nativeApplicationClass = Class.forName("com.apple.eawt.Application");
            // Instantiate through getApplication()
            nativeApplication = nativeApplicationClass.getMethod("getApplication", new Class[0]).invoke(null);

            //nativeApplicationListenerClass = Class.forName("com.apple.eawt.ApplicationListener");

            nativeOpenURIHandlerClass = Class.forName("com.apple.eawt.OpenURIHandler");

        } catch (ClassNotFoundException e) {
            nativeApplication = null;
            return new UnknownApplicationBuilder().build();
        } catch (IllegalAccessException e) {
            return new UnknownApplicationBuilder().build();
        } catch (NoSuchMethodException e) {
            return new UnknownApplicationBuilder().build();
        } catch (InvocationTargetException e) {
            return new UnknownApplicationBuilder().build();
        } catch (MalformedURLException e) {
            return new UnknownApplicationBuilder().build();
        }
        
        // Must have successfully configured the EAWT library to be here
        
        MacApplication macApplication = new MacApplication();
        macApplication.setApplication(nativeApplication);
        macApplication.setOpenURIHandlerClass(nativeOpenURIHandlerClass);
        macApplication.addOpenURIHandler(new DefaultOpenURIHandler());
        
        return macApplication;

    }


}

