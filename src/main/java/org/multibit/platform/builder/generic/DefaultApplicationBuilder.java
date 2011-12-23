package org.multibit.platform.builder.generic;

import org.multibit.platform.GenericApplicationSpecification;

/**
 * <p>Builder to provide the following to {@link org.multibit.platform.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Builds a particular variant of the {@link org.multibit.platform.GenericApplication} suitable for the current platform</li>
 * </ul>
 *
 * @since 0.2.0
 *         
 */
public class DefaultApplicationBuilder {
    public DefaultApplication build(GenericApplicationSpecification specification) {
        return new DefaultApplication();
    }
}
