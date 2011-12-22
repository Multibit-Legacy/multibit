package org.multibit.protcolhandler.builders;

import org.junit.Test;
import org.multibit.protocolhandler.GenericApplication;
import org.multibit.protocolhandler.GenericApplicationFactory;

import static org.junit.Assert.assertNotNull;

public class GenericApplicationFactoryTest {

    /**
     * This test is build specific so is ignored by default
     */
    @Test
    public void testMac() {
        GenericApplication testObject = GenericApplicationFactory.INSTANCE.buildGenericApplication();

        assertNotNull(testObject);

    }
}
