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
import org.junit.Test;
import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Constants;
import org.multibit.CreateControllers;
import org.multibit.file.FileHandler;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.MultiBitService;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.viewsystem.simple.SimpleViewSystem;
import org.multibit.viewsystem.swing.action.ActionTestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Functional test to check that replay from the genesis block works ok.
 * 
 * See bug report: https://github.com/jim618/multibit/issues/21
 * 
 * @author jim
 * 
 */
public class GenesisBlockReplayTest extends TestCase {

    private static final Logger log = LoggerFactory.getLogger(GenesisBlockReplayTest.class);

    private SimpleDateFormat formatter;

    @Test
    public void testReplayFromGenesisBlock() throws Exception {

        // Get the system property runFunctionalTest to see if the functional tests need running.
        String runFunctionalTests = System.getProperty(Constants.RUN_FUNCTIONAL_TESTS_PARAMETER);
        if (Boolean.TRUE.toString().equalsIgnoreCase(runFunctionalTests)) {

            // Date format is UTC with century, T time separator and Z for UTC timezone.
            formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
            formatter.setTimeZone(TimeZone.getTimeZone("UTC"));

            File multiBitDirectory = createMultiBitRuntime();

            // Set the application data directory to be the one we just created.
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator(multiBitDirectory);

            // Create MultiBit controller.
            final CreateControllers.Controllers controllers = CreateControllers.createControllers(applicationDataDirectoryLocator);

            log.debug("Creating Bitcoin service");
            // Create the MultiBitService that connects to the dogecoin network.
            MultiBitService multiBitService = new MultiBitService(controllers.bitcoinController);
            controllers.bitcoinController.setMultiBitService(multiBitService);

            // Add the simple view system (no Swing).
            SimpleViewSystem simpleViewSystem = new SimpleViewSystem();
            controllers.coreController.registerViewSystem(simpleViewSystem);
            
            ReplayManager.INSTANCE.initialise(controllers.bitcoinController, true);

            //
            // MultiBit runtime is now setup and running.
            //

            // Wait for a peer connection.
            log.debug("Waiting for peer connection. . . ");
            while (!simpleViewSystem.isOnline()) {
                Thread.sleep(1000);
            }
            log.debug("Now online.");

            // Create a new  wallet and put it in the model as the active wallet.
            ActionTestUtils.createNewActiveWallet(controllers.bitcoinController, "testReplayFromGenesisBlock", false,
                    null);

            log.debug("Replaying blockchain from genesis block");
            List<WalletData> perWalletModelDataList = new ArrayList<WalletData>();
            perWalletModelDataList.add(controllers.bitcoinController.getModel().getActivePerWalletModelData());
            ReplayTask replayTask = new ReplayTask(perWalletModelDataList, null, 0);
            ReplayManager.INSTANCE.offerReplayTask(replayTask);

            // Run for a minute.
            log.debug("Twiddling thumbs for 60 seconds ...");
            Thread.sleep(60000);
            log.debug("... 60 seconds later later.");

            // Check the blockstore has added the downloaded blocks.
            assertNotNull("No multiBitService after replay", multiBitService);
            assertNotNull("No blockStore after replay",  multiBitService.getBlockStore());
            //assertNotNull("No blockStore file after replay",  multiBitService.getBlockStore().getFile());
            //assertTrue("Block size is too short", BLOCKSIZE_AFTER_REPLAY <=  multiBitService.getBlockStore().getFile().length());

            // Tidy up.
            try {
                if (multiBitService.getPeerGroup() != null) {
                    multiBitService.getPeerGroup().stop();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            log.debug("Not running functional test: GenesisBlockReplayTest#testReplayFromGenesisBlock. Add '-DrunFunctionalTests=true' to run");
        }
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
