/**
 * Copyright 2012 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.model;

import junit.framework.TestCase;
import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

public class ModelTest extends TestCase {

    @Test
    public void testPropertiesRoundTrip() throws Exception {
        String FEE_PER_KB_1 = "5000";
        String FEE_PER_KB_2 = "6000";

        // Create runtime
        final File multibitDirectory = createMultiBitRuntime();

        final ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multibitDirectory);

        // Create MultiBit controller.
        final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);
        final BitcoinController controller = controllers.bitcoinController;

        final BitcoinModel model = controller.getModel();

        // Get the feePerKB property, which will be missing

        assertTrue(model.getUserPreference(CoreModel.FEE_PER_KB) == null);

        // Set the feePerKB
        model.setUserPreference(CoreModel.FEE_PER_KB, FEE_PER_KB_1);

        // Check changed
        assertTrue(FEE_PER_KB_1.equals(model.getUserPreference(CoreModel.FEE_PER_KB)));

        // Persist user preferences to multibit.properties
        FileHandler.writeUserPreferences(controller);

        // Set the feePerKB to something different
        model.setUserPreference(CoreModel.FEE_PER_KB, FEE_PER_KB_2);
        assertTrue(FEE_PER_KB_2.equals(model.getUserPreference(CoreModel.FEE_PER_KB)));

        // Reload the preferences
        Properties reloadedProperties = FileHandler.loadUserPreferences(new ApplicationDataDirectoryLocator(multibitDirectory));

        // The user preference should be the persisted one
        assertTrue(FEE_PER_KB_1.equals(reloadedProperties.getProperty(CoreModel.FEE_PER_KB)));
    }

    /**
        * Create a working, portable runtime of MultiBit in a temporary directory.
        *
        * @return the temporary directory the multibit runtime has been created in
        */
       private File createMultiBitRuntime() throws IOException {
           File multiBitDirectory = FileHandler.createTempDirectory("multibit");
           String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

           System.out.println("Building MultiBit runtime in : " + multiBitDirectory.getAbsolutePath());

           // Create an empty multibit.properties.
           File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multibit.properties");
           multibitProperties.createNewFile();
           multibitProperties.deleteOnExit();

           // Copy in the checkpoints stored in git - this is in source/main/resources/.
           File multibitCheckpoints = new File(multiBitDirectoryPath + File.separator + "multibit.checkpoints");
           FileHandler.copyFile(new File("./src/main/resources/multibit.checkpoints"), multibitCheckpoints);
           multibitCheckpoints.deleteOnExit();

           return multiBitDirectory;
       }

}
