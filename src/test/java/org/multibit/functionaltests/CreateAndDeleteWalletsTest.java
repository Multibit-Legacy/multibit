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
package org.multibit.functionaltests;

import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.multibit.viewsystem.swing.action.CreateWalletSubmitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;

/**
 * functional test to check that wallets can be created and deleted ok
 * 
 * @author jim
 * 
 */
public class CreateAndDeleteWalletsTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(CreateAndDeleteWalletsTest.class);

    private static File multiBitDirectory;

    private static BitcoinController controller;

    private static SimpleViewSystem simpleViewSystem;

    @Before
    @Override
    public void setUp() throws IOException {
        // Get the system property runFunctionalTest to see if the functional
        // tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            multiBitDirectory = createMultiBitRuntime();

            // set the application data directory to be the one we just created
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);

            // Create MultiBit controller.
            final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);
            controller = controllers.bitcoinController;

            log.debug("Creating Bitcoin service");
            // create the MultiBitService that connects to the dogecoin network
            MultiBitService multiBitService = new MultiBitService(controller);
            controller.setMultiBitService(multiBitService);

            // add the simple view system (no Swing)
            simpleViewSystem = new SimpleViewSystem();
            controllers.coreController.registerViewSystem(simpleViewSystem);

            // MultiBit runtime is now setup and running
            // Wait a little while to get two connections.
            try {
                Thread.sleep(8000);
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    @Test
    public void testCreateAndDeleteWalletsWithActions() throws Exception {
        // Get the system property runFunctionalTest to see if the functional
        // tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            String test1WalletPath = multiBitDirectory.getAbsolutePath() + File.separator + "actionTest1.wallet";
            String test2WalletPath = multiBitDirectory.getAbsolutePath() + File.separator + "actionTest2.wallet";

            // initially there is a blank WalletData
            assertEquals(1, controller.getModel().getPerWalletModelDataList().size());

            // create test1 wallet
            CreateWalletSubmitAction createNewWalletAction = new CreateWalletSubmitAction(controller, null, null);
            createNewWalletAction.createNewWallet(test1WalletPath);
            Thread.sleep(4000);
            assertEquals(1, controller.getModel().getPerWalletModelDataList().size());

            // create test2 wallet
            createNewWalletAction.createNewWallet(test2WalletPath);
            Thread.sleep(4000);
            assertEquals(2, controller.getModel().getPerWalletModelDataList().size());
        }
    }

    /**
     * Create a working, portable runtime of MultiBit in a temporary directory.
     * 
     * @return the temporary directory the multibit runtime has been created in
     */
    private File createMultiBitRuntime() throws IOException {
        File multiBitDirectory = FileHandler.createTempDirectory("CreateAndDeleteWalletsTest");
        String multiBitDirectoryPath = multiBitDirectory.getAbsolutePath();

        System.out.println("Building MultiDoge runtime in : " + multiBitDirectory.getAbsolutePath());

        // Create an empty multibit.properties.
        File multibitProperties = new File(multiBitDirectoryPath + File.separator + "multidoge.properties");
        multibitProperties.createNewFile();
        multibitProperties.deleteOnExit();

        // Copy in the checkpoints stored in git - this is in source/main/resources/.
        File multibitCheckpoints = new File(multiBitDirectoryPath + File.separator + "multidoge.checkpoints");
        FileHandler.copyFile(new File("./src/main/resources/multidoge.checkpoints"), multibitCheckpoints);
        multibitCheckpoints.deleteOnExit();

        return multiBitDirectory;
    }
}
