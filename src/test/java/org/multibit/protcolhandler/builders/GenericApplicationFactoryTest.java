package org.multibit.protcolhandler.builders;

import org.junit.Test;
import org.multibit.platform.GenericApplication;
import org.multibit.platform.GenericApplicationFactory;
import org.multibit.platform.GenericApplicationSpecification;

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
