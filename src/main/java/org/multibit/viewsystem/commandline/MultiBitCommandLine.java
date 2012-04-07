/**
 * Copyright 2011 multibit.org
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
package org.multibit.viewsystem.commandline;

import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.Properties;

import org.multibit.ApplicationDataDirectoryLocator;
import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.ViewSystem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Experimental command line version of MultiBit
 * 
 * @author jim
 */
public class MultiBitCommandLine {
    private static final Logger log = LoggerFactory.getLogger(MultiBitCommandLine.class);

    /**
     * Start MultiBit in command line mode
     */
    public static void main(String args[]) {
        log.info("Starting MultiBitCommandLine");

        CommandLineViewSystem commandLineViewSystem = null;

        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();

        // load up the user preferences
        Properties userPreferences = FileHandler.loadUserPreferences(applicationDataDirectoryLocator);

        // create the controller
        final MultiBitController controller = new MultiBitController(applicationDataDirectoryLocator);

        // if test or production is not specified, default to production
        String testOrProduction = userPreferences.getProperty(MultiBitModel.TEST_OR_PRODUCTION_NETWORK);
        if (testOrProduction == null) {
            testOrProduction = MultiBitModel.PRODUCTION_NETWORK_VALUE;
            userPreferences.put(MultiBitModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
        }
        boolean useTestNet = MultiBitModel.TEST_NETWORK_VALUE.equals(testOrProduction);
        log.debug("useTestNet = {}", useTestNet);

        Localiser localiser;
        String userLanguageCode = userPreferences.getProperty(MultiBitModel.USER_LANGUAGE_CODE);
        log.debug("userLanguageCode = {}", userLanguageCode);

        if (userLanguageCode == null) {
            // initial install - no language info supplied - see if we can use
            // the user default, else Localiser will set it to English
            localiser = new Localiser(Locale.getDefault());

            userPreferences.setProperty(MultiBitModel.USER_LANGUAGE_CODE, localiser.getLocale().getLanguage());
        } else {
            if (MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
                localiser = new Localiser(Locale.getDefault());
            } else {
                localiser = new Localiser(new Locale(userLanguageCode));
            }
        }
        controller.setLocaliser(localiser);

        log.debug("Creating model");

        // create the model
        MultiBitModel model = new MultiBitModel(controller, userPreferences);

        log.debug("Creating views");
        commandLineViewSystem = new CommandLineViewSystem(controller);

        log.debug("Registering with controller");
        controller.registerViewSystem(commandLineViewSystem);

        log.debug("Creating Bitcoin service");
        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(useTestNet, controller);
        controller.setMultiBitService(multiBitService);

        log.debug("Locating wallets");
        // find the active wallet filename in the multibit.properties
        String activeWalletFilename = userPreferences.getProperty(MultiBitModel.ACTIVE_WALLET_FILENAME);

        // load up all the wallets
        String numberOfWalletsAsString = userPreferences.getProperty(MultiBitModel.NUMBER_OF_WALLETS);
        if (numberOfWalletsAsString == null || "".equals(numberOfWalletsAsString)) {
            // if this is missing then there is just the one wallet (old format
            // properties or user has just started up for the first time)
            try {
                // activeWalletFilename may be null on first time startup
                controller.addWalletFromFilename(activeWalletFilename);
                List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
                if (perWalletModelDataList != null && !perWalletModelDataList.isEmpty()) {
                    activeWalletFilename = perWalletModelDataList.get(0).getWalletFilename();
                    controller.getModel().setActiveWalletByFilename(activeWalletFilename);
                    log.debug("Created/loaded wallet '" + activeWalletFilename + "'");
                    controller.updateStatusLabel(controller.getLocaliser().getString("multiBit.createdWallet",
                            new Object[] { activeWalletFilename }));
                }
            } catch (IOException e) {
                String message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                        new Object[] { activeWalletFilename, e.getMessage() });
                controller.updateStatusLabel(message);
                log.error(message);
            }
        } else {
            try {
                int numberOfWallets = Integer.parseInt(numberOfWalletsAsString);

                if (numberOfWallets > 0) {
                    for (int i = 1; i <= numberOfWallets; i++) {
                        // load up ith wallet filename
                        String loopWalletFilename = userPreferences.getProperty(MultiBitModel.WALLET_FILENAME_PREFIX + i);
                        log.debug("Loading wallet from '{}'", loopWalletFilename);
                        controller.updateStatusLabel(controller.getLocaliser().getString("multiBit.openingWallet",
                                new Object[] { loopWalletFilename }));
                        try {
                            if (activeWalletFilename != null && activeWalletFilename.equals(loopWalletFilename)) {
                                controller.addWalletFromFilename(loopWalletFilename);
                                controller.getModel().setActiveWalletByFilename(loopWalletFilename);
                            } else {
                                controller.addWalletFromFilename(loopWalletFilename);
                            }
                            controller.updateStatusLabel(controller.getLocaliser().getString("multiBit.openingWalletIsDone",
                                    new Object[] { loopWalletFilename }));
                        } catch (IOException e) {
                            String message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                                    new Object[] { activeWalletFilename, e.getMessage() });
                            controller.updateStatusLabel(message);
                            log.error(message);
                        }
                    }
                }

            } catch (NumberFormatException nfe) {
                // carry on
            }
        }

        // Indicate to the application that startup has completed
        controller.setApplicationStarting(false);

        log.debug("Downloading blockchain");
        multiBitService.downloadBlockChain();

        controller.displayView(controller.getCurrentView());
        
        // show the command line help
        try {
            commandLineViewSystem.getMultiBitTool().processLine(new String[]{"HELP"});
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
