package org.multibit;

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

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.network.FileHandler;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.util.Locale;
import java.util.Properties;

/**
 * Main MultiBit entry class
 *
 * @author jim
 */
public class MultiBit {
    private static Logger logger;

    /**
     * start multibit user interface
     */
    public static void main(String args[]) {
        // initialise log4j
        logger = LoggerFactory.getLogger(MultiBit.class);

        if (args != null && args.length > 0) {
            for (int i = 0; i < args.length; i++) {
                logger.info("Started with args[{}]: '{}'", i, args[i]);
            }
        }
        
        logger.info("Stage 1 app locator");

        ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();

        logger.info("Stage 2 the rest");

        // load up the user preferences
        Properties userPreferences = FileHandler.loadUserPreferences(applicationDataDirectoryLocator);

        // create the controller
        MultiBitController controller = new MultiBitController(userPreferences, applicationDataDirectoryLocator);

        // if test or production is not specified, default to production
        String testOrProduction = userPreferences.getProperty(MultiBitModel.TEST_OR_PRODUCTION_NETWORK);
        if (testOrProduction == null) {
            testOrProduction = MultiBitModel.PRODUCTION_NETWORK_VALUE;
            userPreferences.put(MultiBitModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
        }
        boolean useTestNet = MultiBitModel.TEST_NETWORK_VALUE.equals(testOrProduction);
        logger.debug("useTestNet = " + useTestNet);

        Localiser localiser;
        String userLanguageCode = userPreferences.getProperty(MultiBitModel.USER_LANGUAGE_CODE);
        logger.debug("userLanguageCode = " + userLanguageCode);

        if (userLanguageCode == null) {
            // no language info supplied - set to English
            userPreferences.setProperty(MultiBitModel.USER_LANGUAGE_CODE, Locale.ENGLISH.getLanguage());
            localiser = new Localiser(Locale.ENGLISH);
        } else {
            if (MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
                localiser = new Localiser(Locale.getDefault());
            } else {
                localiser = new Localiser(new Locale(userLanguageCode));
            }
        }
        controller.setLocaliser(localiser);

        // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel(controller, userPreferences);
        controller.setModel(model);

        try {
            // Set System L&F
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (UnsupportedLookAndFeelException e) {
            // carry on
        } catch (ClassNotFoundException e) {
            // carry on
        } catch (InstantiationException e) {
            // carry on
        } catch (IllegalAccessException e) {
            // carry on
        }

        // create the view systems
        // add the swing view system
        ViewSystem swingViewSystem = new MultiBitFrame(controller);
        controller.registerViewSystem(swingViewSystem);

        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(useTestNet, controller);
        controller.setMultiBitService(multiBitService);

        // find the active wallet filename in the multibit.properties
        String activeWalletFilename = userPreferences.getProperty(MultiBitModel.ACTIVE_WALLET_FILENAME);

        // load up all the wallets
        String numberOfWalletsAsString = userPreferences.getProperty(MultiBitModel.NUMBER_OF_WALLETS);
        if (numberOfWalletsAsString == null || "".equals(numberOfWalletsAsString)) {
            // if this is missing then there is just the one wallet (old format
            // properties)
            controller.addWalletFromFilename(activeWalletFilename);
            controller.getModel().setActiveWalletByFilename(activeWalletFilename);
            controller.fireWalletChanged();
            controller.fireDataChanged();
        } else {
            try {
                int numberOfWallets = Integer.parseInt(numberOfWalletsAsString);

                if (numberOfWallets > 0) {
                    for (int i = 1; i <= numberOfWallets; i++) {
                        // load up ith wallet filename
                        String loopWalletFilename = userPreferences.getProperty(MultiBitModel.WALLET_FILENAME_PREFIX + i);
                        if (activeWalletFilename != null && activeWalletFilename.equals(loopWalletFilename)) {
                            controller.addWalletFromFilename(loopWalletFilename);
                            controller.getModel().setActiveWalletByFilename(loopWalletFilename);

                        } else {
                            controller.addWalletFromFilename(loopWalletFilename);
                        }
                    }
                    controller.fireNewWalletCreated();
                    controller.fireWalletChanged();
                    controller.fireDataChanged();
                }
            } catch (NumberFormatException nfe) {
                // carry on
            }
        }

        // display the next view
        controller.displayNextView(ViewSystem.NEW_VIEW_IS_SIBLING_OF_PREVIOUS);

        // see if the user wants to connect to a single node
        multiBitService.downloadBlockChain();
    }
}
