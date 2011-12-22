package org.multibit.protocolhandler.builders;

import org.multibit.protocolhandler.GenericApplication;

/**
 * <p>GenericApplication to provide the following to {@link org.multibit.protocolhandler.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Provision of methods for the given platform</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public class UnknownApplication implements GenericApplication {

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
