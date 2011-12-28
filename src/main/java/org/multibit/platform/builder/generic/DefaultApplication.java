package org.multibit.platform.builder.generic;

import org.multibit.platform.GenericApplication;

/**
 * <p>GenericApplication to provide the following to {@link org.multibit.platform.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Provision of methods for the given platform</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class DefaultApplication implements GenericApplication {

    @Override
    public boolean isMac() {
        return false;
    }

    @Override
    public boolean isLinux() {
        return false;
    }

    @Override
    public boolean isWindows() {
        return false;
    }
}
