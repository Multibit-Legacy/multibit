package org.multibit.protocolhandler.builders;

import org.multibit.protocolhandler.GenericApplicationSpecification;

/**
 * <p>Builder to provide the following to {@link org.multibit.protocolhandler.GenericApplicationFactory}:</p>
 * <ul>
 * <li>Builds a particular variant of the {@link org.multibit.protocolhandler.GenericApplication} suitable for the current platform</li>
 * </ul>
 *
 * @since 0.2.0
 *         
 */
public class SolarisApplicationBuilder {
    public UnknownApplication build(GenericApplicationSpecification specification) {
        return new UnknownApplication();
    }
}
