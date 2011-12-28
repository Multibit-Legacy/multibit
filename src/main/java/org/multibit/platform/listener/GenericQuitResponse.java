package org.multibit.platform.listener;

/**
 * <p>Generic response to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Proxies any native handling code for quit response</li>
 * </ul>
 *
 * @since 0.3.0
 */
public interface GenericQuitResponse extends GenericEvent {

    /**
     * Call if quit should be stopped due to application tasks
     */
    void cancelQuit();

    /**
     * Call if quit is OK to proceed
     */
    void performQuit();
}
