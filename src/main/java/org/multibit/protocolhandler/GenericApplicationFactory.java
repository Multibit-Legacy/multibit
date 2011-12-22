package org.multibit.protocolhandler;

import org.multibit.protocolhandler.builders.*;

/**
 * <p>Factory to provide the following to application:</p>
 * <ul>
 * <li>Builds a {@link GenericApplication} appropriate for the execution platform</li>
 * </ul>
 *
 * @since 0.2.0
 *         
 */
public enum GenericApplicationFactory {
    INSTANCE;

    /**
     * @return A {@link GenericApplication} appropriate for the current execution platform
     */
    public GenericApplication buildGenericApplication() {
        // Determine the current platform
        if (OSUtils.isMac()) {
            return buildMacApplication();
        }
        if (OSUtils.isLinux()) {
            return buildLinuxApplication();
        }
        if (OSUtils.isSolaris()) {
            return buildSolarisApplication();
        }
        if (OSUtils.isUnix()) {
            return buildUnixApplication();
        }
        if (OSUtils.isWindows()) {
            return buildWindowsApplication();
        }
        return buildUnknownApplication();
    }

    private GenericApplication buildLinuxApplication() {
        return new LinuxApplicationBuilder().build();
    }

    private GenericApplication buildMacApplication() {
        return new MacApplicationBuilder().build();
    }

    private GenericApplication buildSolarisApplication() {
        return new SolarisApplicationBuilder().build();
    }

    private GenericApplication buildUnixApplication() {
        return new UnixApplicationBuilder().build();
    }

    private GenericApplication buildWindowsApplication() {
        return new WindowsApplicationBuilder().build();
    }

    private GenericApplication buildUnknownApplication() {
        return new UnknownApplicationBuilder().build();
    }

}
