package org.multibit.protcolhandler.builders;

import org.junit.Test;
import org.multibit.protocolhandler.GenericApplication;
import org.multibit.protocolhandler.GenericApplicationFactory;
import org.multibit.protocolhandler.GenericApplicationSpecification;

import static org.junit.Assert.assertNotNull;

public class GenericApplicationFactoryTest {

    /**
     * This test is build specific so is ignored by default
     */
    @Test
    public void testMac() {
        GenericApplicationSpecification specification = new GenericApplicationSpecification();
        GenericApplication testObject = GenericApplicationFactory.INSTANCE.buildGenericApplication(specification);

        assertNotNull(testObject);

    }
}
