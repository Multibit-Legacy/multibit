package org.multibit.platform.builder.win;

import org.multibit.platform.GenericApplicationSpecification;
import org.multibit.platform.builder.generic.DefaultApplication;

/**
 * <p>Builder to provide the following to {@link org.multibit.platform.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Builds a particular variant of the {@link org.multibit.platform.GenericApplication} suitable for the current platform</li>
 * </ul>
 *
 * @since 0.2.0
 *         
 */
public class WindowsApplicationBuilder {
    public DefaultApplication build(GenericApplicationSpecification specification) {
        return new DefaultApplication();
    }
}
