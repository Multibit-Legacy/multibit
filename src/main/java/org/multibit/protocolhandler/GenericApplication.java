package org.multibit.protocolhandler;

/**
 * <p>Interface to provide the following to {@link OpenURIHandlerApplication}:</p>
 * <ul>
 * <li>Provision of dedicated URI handling methods</li>
 * </ul>
 * <p>Example:</p>
 * <pre>
 * </pre>
 *
 * @since 0.3.0
 *         
 */
public interface GenericApplication {

    /**
     * @return True if the application is running on a Mac AND the Apple Extensions API classes are available.
     */
    boolean isMac();

    /**
     * @return True if the application is running on a Mac AND the Linux Extensions API classes are available.
     */
    boolean isLinux();

    /**
     * @return True if the application is running on a Windows machine AND the Windows Extensions API classes are available.
     */
    boolean isWindows();



}
