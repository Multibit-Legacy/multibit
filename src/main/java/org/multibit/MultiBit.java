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
package org.multibit;

import java.awt.Cursor;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Properties;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.controller.core.CoreController;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.network.AlertManager;
import org.multibit.network.MultiBitCheckpointManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.core.CoreModel;
import org.multibit.model.exchange.ConnectHttps;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.network.MultiBitService;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.platform.GenericApplication;
import org.multibit.platform.GenericApplicationFactory;
import org.multibit.platform.GenericApplicationSpecification;
import org.multibit.platform.listener.GenericOpenURIEvent;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.core.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.core.actions.ExitAction;
import org.multibit.viewsystem.swing.core.components.FontSizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.StoredBlock;
import com.google.bitcoin.core.Wallet;

/**
 * Main MultiBit entry class.
 * 
 * @author jim
 */
public class MultiBit {
    private static final Logger log = LoggerFactory.getLogger(MultiBit.class);

    private static Controller controller = null;
    
    private static CoreController coreController = null;
    private static BitcoinController bitcoinController = null;
    private static ExchangeController exchangeController = null;
    
    public static Controller getController() {
        return controller;
    }
    
    public static CoreController getCoreController() {
        return coreController;
    }
    
    public static BitcoinController getBitcoinController() {
        return bitcoinController;
    }
    
    public static ExchangeController getExchangeController() {
        return exchangeController;
    }
    
    /**
     * Used in testing
     */
    public static void setCoreController(CoreController coreController ) {
        MultiBit.controller = coreController;
        MultiBit.coreController = coreController;
    }
    
    public static void setBitcoinController(BitcoinController bitcoinController) {
        MultiBit.bitcoinController = bitcoinController;
    }
     
     public static void setExchangeController(ExchangeController exchangeController) {
        MultiBit.exchangeController = exchangeController;
    }
    
    /**
     * Start MultiBit user interface.
     * 
     * @param args
     *            String encoding of arguments ([0]= Bitcoin URI)
     */
    @SuppressWarnings("deprecation")
    public static void main(String args[]) {
        log.info("Starting MultiBit at " + (new Date()).toGMTString());

        // Enclosing try to enable graceful closure for unexpected errors.
        try {
            // Set any bespoke system properties.
            try {
                // Fix for Windows / Java 7 / VPN bug.
                System.setProperty("java.net.preferIPv4Stack", "true");
                
                // Fix for version.txt not visible for Java 7 
                System.setProperty ("jsse.enableSNIExtension", "false");                
            } catch (SecurityException se) {
                log.error(se.getClass().getName() + " " + se.getMessage());
            }
            
            ViewSystem swingViewSystem = null;

            ApplicationDataDirectoryLocator applicationDataDirectoryLocator = new ApplicationDataDirectoryLocator();

            // Load up the user preferences.
            Properties userPreferences = FileHandler.loadUserPreferences(applicationDataDirectoryLocator);

            // create the controllers
            coreController = new CoreController(applicationDataDirectoryLocator);
            controller = coreController;
            bitcoinController = new BitcoinController(coreController);
            exchangeController = new ExchangeController(coreController);


            log.info("Configuring native event handling");
            GenericApplicationSpecification specification = new GenericApplicationSpecification();
            specification.getOpenURIEventListeners().add(coreController);
            specification.getPreferencesEventListeners().add(coreController);
            specification.getAboutEventListeners().add(coreController);
            specification.getQuitEventListeners().add(coreController);
            GenericApplication genericApplication = GenericApplicationFactory.INSTANCE.buildGenericApplication(specification);

            log.info("Checking to see if this is the primary MultiBit instance");
            String rawURI = null;
            if (args != null && args.length > 0) {
                rawURI = args[0];
            }
            if (!ApplicationInstanceManager.registerInstance(rawURI)) {
                // Instance already running.
                log.debug("Another instance of MultiBit is already running.  Exiting.");
                System.exit(0);
            }

            final BitcoinController finalController = bitcoinController;
            ApplicationInstanceManager.setApplicationInstanceListener(new ApplicationInstanceListener() {
                @Override
                public void newInstanceCreated(String rawURI) {
                    final String finalRawUri = rawURI;
                    log.debug("New instance of MultiBit detected...");
                    Runnable doProcessCommandLine = new Runnable() {
                        @Override
                        public void run() {
                            processCommandLineURI(finalController, finalRawUri);
                        }
                    };

                    SwingUtilities.invokeLater(doProcessCommandLine);
                }
            });

            Localiser localiser;
            String userLanguageCode = userPreferences.getProperty(CoreModel.USER_LANGUAGE_CODE);
            log.debug("userLanguageCode = {}", userLanguageCode);

            if (userLanguageCode == null) {
                // Initial install - no language info supplied - see if we can
                // use the user default, else Localiser will set it to English.
                localiser = new Localiser(Locale.getDefault());

                userPreferences.setProperty(CoreModel.USER_LANGUAGE_CODE, localiser.getLocale().getLanguage());
            } else {
                if (CoreModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
                    localiser = new Localiser(Locale.getDefault());
                } else {
                    localiser = new Localiser(new Locale(userLanguageCode));
                }
            }
            coreController.setLocaliser(localiser);

            log.debug("Creating model");

            // Create the model.
            // The model is set to the controller.
            {
            final CoreModel coreModel = new CoreModel(userPreferences);
            final BitcoinModel model = new BitcoinModel(coreModel);
            final ExchangeModel exchangeModel = new ExchangeModel(coreModel);
                coreController.setModel(coreModel);
                bitcoinController.setModel(model);
                exchangeController.setModel(exchangeModel);
            }

            // Trust all HTTPS certificates.
            ConnectHttps.trustAllCerts();
            
            // Initialise currency converter.
            CurrencyConverter.INSTANCE.initialise(finalController);
            
            // Initialise replay manager.
            ReplayManager.INSTANCE.initialise(bitcoinController);
            
            log.debug("Setting look and feel");
            try {
                String lookAndFeel = userPreferences.getProperty(CoreModel.LOOK_AND_FEEL);
 
                if (lookAndFeel != null && lookAndFeel != "") {
                    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                        if (lookAndFeel.equalsIgnoreCase(info.getName())) {
                            UIManager.setLookAndFeel(info.getClassName());
                            break;
                        }
                    }
                } else {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            } catch (UnsupportedLookAndFeelException e) {
                // carry on
            } catch (ClassNotFoundException e) {
                // carry on
            } catch (InstantiationException e) {
                // carry on
            } catch (IllegalAccessException e) {
                // carry on
            }

            // Initialise singletons.
            ColorAndFontConstants.init();
            FontSizer.INSTANCE.initialise(controller);
            CurrencyConverter.INSTANCE.initialise(finalController);
            
            // Check the fee is between the lower and upper bounds - set to default fee if not.
            String sendFeeString = controller.getModel().getUserPreference(BitcoinModel.SEND_FEE);
            boolean setToDefaultFee = false;

            if (sendFeeString == null || sendFeeString == "") {
                setToDefaultFee = true;
            } else {
                CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(sendFeeString);

                if (converterResult.isBtcMoneyValid()) {
                    // Check that the fee is at least the minimum fee and
                    // smaller than the maximum fee.
                    BigInteger feeAsBigInteger = converterResult.getBtcMoney().getAmount().toBigInteger();
                    if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MINIMUM_FEE) < 0) {
                        setToDefaultFee = true;
                    } else {
                        if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MAXIMUM_FEE) >= 0) {
                            setToDefaultFee = true;
                        }
                    }
                }
            }

            if (setToDefaultFee) {
               sendFeeString = controller.getLocaliser()
                        .bitcoinValueToStringNotLocalised(BitcoinModel.SEND_FEE_DEFAULT, false, false);
               controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, sendFeeString);
            }

            // This is when the GUI is first displayed to the user.
            log.debug("Creating user interface with initial view : " + controller.getCurrentView());
            swingViewSystem = new MultiBitFrame(coreController, bitcoinController, exchangeController, genericApplication, controller.getCurrentView());

            log.debug("Registering with controller");
            coreController.registerViewSystem(swingViewSystem);

            log.debug("Creating Bitcoin service");
            // Create the MultiBitService that connects to the bitcoin network.
            MultiBitService multiBitService = new MultiBitService(bitcoinController);
            bitcoinController.setMultiBitService(multiBitService);

            log.debug("Locating wallets");
            // Find the active wallet filename in the multibit.properties.
            String activeWalletFilename = userPreferences.getProperty(BitcoinModel.ACTIVE_WALLET_FILENAME);

            // Get the number of the early wallets - these are serialised and protobuf2
            String numberOfEarlyWalletsAsString = userPreferences.getProperty(BitcoinModel.NUMBER_OF_EARLY_WALLETS);
            log.debug("When loading early wallets, there were " + numberOfEarlyWalletsAsString);

            // Get the number of the protobuf3 wallets
            String numberOfProtobuf3WalletsAsString = userPreferences.getProperty(BitcoinModel.NUMBER_OF_PROTOBUF3_WALLETS);
            log.debug("When loading protobuf3 wallets, there were " + numberOfProtobuf3WalletsAsString);

            boolean useFastCatchup = false;

            if (numberOfEarlyWalletsAsString == null || "".equals(numberOfEarlyWalletsAsString)) {
                // If this is missing then there is just the one wallet (old format
                // properties or user has just started up for the first time).
                useFastCatchup = true;
                boolean thereWasAnErrorLoadingTheWallet = false;

                try {
                    // ActiveWalletFilename may be null on first time startup.
                    bitcoinController.addWalletFromFilename(activeWalletFilename);
                    List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
                    if (perWalletModelDataList != null && !perWalletModelDataList.isEmpty()) {
                        activeWalletFilename = perWalletModelDataList.get(0).getWalletFilename();
                        bitcoinController.getModel().setActiveWalletByFilename(activeWalletFilename);
                        log.debug("Created/loaded wallet '" + activeWalletFilename + "'");
                        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString(
                                "multiBit.createdWallet", new Object[] { activeWalletFilename })));
                    }
                } catch (WalletLoadException e) {
                    String message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                            new Object[] { activeWalletFilename, e.getMessage() });
                    MessageManager.INSTANCE.addMessage(new Message(message));
                    log.error(message);
                    thereWasAnErrorLoadingTheWallet = true;
                } catch (WalletVersionException e) {
                    String message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                            new Object[] { activeWalletFilename, e.getMessage() });
                    MessageManager.INSTANCE.addMessage(new Message(message));
                    log.error(message);
                    thereWasAnErrorLoadingTheWallet = true;
                } catch (IOException e) {
                    String message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                            new Object[] { activeWalletFilename, e.getMessage() });
                    MessageManager.INSTANCE.addMessage(new Message(message));
                    log.error(message);
                    thereWasAnErrorLoadingTheWallet = true;
                } finally {
                    if (thereWasAnErrorLoadingTheWallet) {
                        // Clear the backup wallet filename - this prevents it being automatically overwritten.
                        if (bitcoinController.getModel().getActiveWalletWalletInfo() != null) {
                            bitcoinController.getModel().getActiveWalletWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, "");
                        }
                    }
                    if (swingViewSystem instanceof MultiBitFrame) {
                        ((MultiBitFrame) swingViewSystem).getWalletsView().initUI();
                        ((MultiBitFrame) swingViewSystem).getWalletsView().displayView(DisplayHint.COMPLETE_REDRAW);
                    }
                    controller.fireDataChangedUpdateNow();
                }
            } else {
                try {
                    List<String> walletFilenamesToLoad = new ArrayList<String>();
                    try {
                        int numberOfEarlyWallets = Integer.parseInt(numberOfEarlyWalletsAsString);
                        if (numberOfEarlyWallets > 0) {
                            for (int i = 1; i <= numberOfEarlyWallets; i++) {
                                // look up ith wallet filename
                                String loopWalletFilename = userPreferences.getProperty(BitcoinModel.EARLY_WALLET_FILENAME_PREFIX + i);
                                if (!walletFilenamesToLoad.contains(loopWalletFilename)) {
                                    walletFilenamesToLoad.add(loopWalletFilename);
                                }
                            }
                        }
                    } catch (NumberFormatException nfe) {
                        // Carry on.
                    }
                    try {
                        int numberOfProtobuf3Wallets = Integer.parseInt(numberOfProtobuf3WalletsAsString);
                        if (numberOfProtobuf3Wallets > 0) {
                            for (int i = 1; i <= numberOfProtobuf3Wallets; i++) {
                                // look up ith wallet filename
                                String loopWalletFilename = userPreferences.getProperty(BitcoinModel.PROTOBUF3_WALLET_FILENAME_PREFIX + i);
                                if (!walletFilenamesToLoad.contains(loopWalletFilename)) {
                                    walletFilenamesToLoad.add(loopWalletFilename);
                                }
                            }
                        }
                    } catch (NumberFormatException nfe) {
                        // Carry on.
                    }
                    
                    // Load up the order the wallets are to appear in.
                    // There may be wallets in this list of types from the future but only load wallets we know about
                    boolean haveWalletOrder = false;
                    List<String> walletFilenameOrder = new ArrayList<String>();
                    try {
                        String walletOrderTotalAsString = userPreferences.getProperty(BitcoinModel.WALLET_ORDER_TOTAL);
                        log.debug("When loading the wallet orders, there were " + walletOrderTotalAsString);

                        int walletOrderTotal = Integer.parseInt(walletOrderTotalAsString);
                        if (walletOrderTotal > 0) {
                            haveWalletOrder = true;
                            for (int i = 1; i <= walletOrderTotal; i++) {
                                // add the wallet filename order
                                String loopWalletFilename = userPreferences.getProperty(BitcoinModel.WALLET_ORDER_PREFIX + i);
                                if (!walletFilenameOrder.contains(loopWalletFilename)) {
                                    walletFilenameOrder.add(loopWalletFilename);
                                }
                            }
                        }
                    } catch (NumberFormatException nfe) {
                        // Carry on.
                    }
                    
                    List<String> actualOrderToLoad = new ArrayList<String>();
                    if (haveWalletOrder) {
                        for (String orderWallet : walletFilenameOrder) {
                            if (walletFilenamesToLoad.contains(orderWallet)) {
                                // Add it.
                                actualOrderToLoad.add(orderWallet);
                            }
                        }
                        // There may be some extras so add them to the end.
                        for (String loadWallet : walletFilenamesToLoad) {
                            if (!walletFilenameOrder.contains(loadWallet)) {
                                // Add it.
                                actualOrderToLoad.add(loadWallet);
                            }
                        }                        
                    } else {
                        // Just load all the wallets, early then later.
                        for (String loadWallet : walletFilenamesToLoad) {
                            if (!actualOrderToLoad.contains(loadWallet)) {
                                // Add it.
                                actualOrderToLoad.add(loadWallet);
                            }
                        }
                    }
                    

                    if (actualOrderToLoad.size() > 0) {
                        boolean thereWasAnErrorLoadingTheWallet = false;

                        ((MultiBitFrame) swingViewSystem).setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                        for (String actualOrder : actualOrderToLoad) {
                            log.debug("Loading wallet from '{}'", actualOrder);
                            Message message = new Message(controller.getLocaliser().getString("multiBit.openingWallet",
                                    new Object[] { actualOrder }));
                            message.setShowInStatusBar(false);
                            MessageManager.INSTANCE.addMessage(message);
                            try {
                                if (activeWalletFilename != null && activeWalletFilename.equals(actualOrder)) {
                                    bitcoinController.addWalletFromFilename(actualOrder);
                                    bitcoinController.getModel().setActiveWalletByFilename(actualOrder);
                                } else {
                                    bitcoinController.addWalletFromFilename(actualOrder);
                                }
                                Message message2 = new Message(controller.getLocaliser().getString("multiBit.openingWalletIsDone",
                                        new Object[] { actualOrder }));
                                message2.setShowInStatusBar(false);
                                MessageManager.INSTANCE.addMessage(message2);
                            } catch (WalletLoadException e) {
                                message = new Message(controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                                        new Object[] { actualOrder, e.getMessage() }));
                                MessageManager.INSTANCE.addMessage(message);
                                log.error(message.getText());
                                thereWasAnErrorLoadingTheWallet = true;
                            } catch (WalletVersionException e) {
                                message = new Message(controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                                        new Object[] { actualOrder, e.getMessage() }));
                                MessageManager.INSTANCE.addMessage(message);
                                log.error(message.getText());
                                thereWasAnErrorLoadingTheWallet = true;
                            } catch (IOException e) {
                                message = new Message(controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded",
                                        new Object[] { actualOrder, e.getMessage() }));
                                MessageManager.INSTANCE.addMessage(message);
                                log.error(message.getText());
                                thereWasAnErrorLoadingTheWallet = true;
                            }
                            
                            if (thereWasAnErrorLoadingTheWallet) {
                                WalletData loopData = bitcoinController.getModel().getPerWalletModelDataByWalletFilename(actualOrder);
                                if (loopData != null) {
                                    // Clear the backup wallet filename - this prevents it being automatically overwritten.
                                    if (loopData.getWalletInfo() != null) {
                                        loopData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, "");
                                    }
                                }
                            }
                        }
                    }

                } catch (NumberFormatException nfe) {
                    // Carry on.
                } finally {
                    if (swingViewSystem instanceof MultiBitFrame) {
                        ((MultiBitFrame) swingViewSystem).getWalletsView().initUI();
                        ((MultiBitFrame) swingViewSystem).getWalletsView().displayView(DisplayHint.COMPLETE_REDRAW);
                    }
                    controller.fireDataChangedUpdateNow();

                    ((MultiBitFrame) swingViewSystem).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                }
            }

            log.debug("Checking for Bitcoin URI on command line");
            // Check for a valid entry on the command line (protocol handler).
            if (args != null && args.length > 0) {
                for (int i = 0; i < args.length; i++) {
                    log.debug("Started with args[{}]: '{}'", i, args[i]);
                }
                processCommandLineURI(bitcoinController, args[0]);
            } else {
                log.debug("No Bitcoin URI provided as an argument");
            }

            // Indicate to the application that startup has completed.
            coreController.setApplicationStarting(false);

            // Check for any pending URI operations.
            bitcoinController.handleOpenURI();

            // Check to see if there is a new version.
            AlertManager.INSTANCE.initialise(bitcoinController, (MultiBitFrame) swingViewSystem);
            AlertManager.INSTANCE.checkVersion();
            
            log.debug("Downloading blockchain");
            if (useFastCatchup) {
                long earliestTimeSecs = bitcoinController.getModel().getActiveWallet().getEarliestKeyCreationTime();
                bitcoinController.getMultiBitService().getPeerGroup().setFastCatchupTimeSecs(earliestTimeSecs);
                log.debug("Using FastCatchup for blockchain sync with time of " + (new Date(earliestTimeSecs)).toString());
            }
            
            // Work out the late date/ block the wallets saw to see if it needs syncing
            // or if we can use regular downloading
            int currentChainHeight = -1;
            if (bitcoinController.getMultiBitService().getChain() != null) {
                if (bitcoinController.getMultiBitService().getChain().getChainHead() != null) {
                    currentChainHeight = bitcoinController.getMultiBitService().getChain().getChainHead().getHeight();
                }
            }
            
            log.debug("The current chain height is " + currentChainHeight);
            
            List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();
            boolean needToSync = false;
            int syncFromHeight = -1;

            List<WalletData> replayPerWalletModelList = new ArrayList<WalletData>();
            if (perWalletModelDataList != null) {
                for (WalletData perWalletModelData : perWalletModelDataList) {
                    Wallet wallet = perWalletModelData.getWallet();
                    if (wallet != null) {
                        int lastBlockSeenHeight = wallet.getLastBlockSeenHeight();
                        log.debug("For wallet '" + perWalletModelData.getWalletFilename() + " the lastBlockSeenHeight was "
                                + lastBlockSeenHeight);

                        // Check if we have both the lastBlockSeenHeight and the currentChainHeight.
                        if (lastBlockSeenHeight > 0 && currentChainHeight > 0) {
                            if (lastBlockSeenHeight >= currentChainHeight) {
                                // Wallet is at or ahead of current chain - no
                                // need to sync for this wallet.
                            } else {
                                // Wallet is behind the current chain - need to sync.
                                needToSync = true;

                                replayPerWalletModelList.add(perWalletModelData);
                                if (syncFromHeight == -1) {
                                    syncFromHeight = lastBlockSeenHeight;
                                } else {
                                    syncFromHeight = Math.min(syncFromHeight, lastBlockSeenHeight);
                                }
                            }
                        }
                    }
                }
            } 
            log.debug("needToSync = " + needToSync);

            if (needToSync) {
                StoredBlock syncFromStoredBlock = null;

                MultiBitCheckpointManager checkpointManager = bitcoinController.getMultiBitService().getCheckpointManager();
                if (checkpointManager != null) {
                    syncFromStoredBlock = checkpointManager.getCheckpointBeforeOrAtHeight(syncFromHeight);
                }

                ReplayTask replayTask;
                if (syncFromStoredBlock == null) {
                    // Sync from genesis block.
                    replayTask = new ReplayTask(replayPerWalletModelList, null, 0);
                } else {
                    Date syncDate = null;
                    if (syncFromStoredBlock.getHeader() != null) {
                        syncDate = new Date(syncFromStoredBlock.getHeader().getTimeSeconds() * 1000);
                    }
                    replayTask = new ReplayTask(replayPerWalletModelList, syncDate, syncFromStoredBlock.getHeight());
                }
                ReplayManager.INSTANCE.offerReplayTask(replayTask);
            } else {
                // Just sync the blockchain without a replay task being involved.
                multiBitService.downloadBlockChain();
            }
        } catch (Exception e) {
            // An unexcepted, unrecoverable error occurred.
            e.printStackTrace();

            log.error("An unexpected error caused MultiBit to quit.");
            log.error("The error was '" + e.getClass().getCanonicalName() + " " + e.getMessage() + "'");
            e.printStackTrace();
            log.error("Please read http://multibit.org/help_troubleshooting.html for help on troubleshooting.");

            // Try saving any dirty wallets.
            if (controller != null) {
                ExitAction exitAction = new ExitAction(controller, null);
                exitAction.actionPerformed(null);
            }
        }
    }

    static void processCommandLineURI(BitcoinController controller, String rawURI) {
        try {
            // Attempt to detect if the command line URI is valid.
            // Note that this is largely because IE6-8 strip URL encoding
            // when passing in URIs to a protocol handler.
            // However, there is also the chance that anyone could
            // hand-craft a URI and pass
            // it in with non-ASCII character encoding present in the label
            // This a really limited approach (no consideration of
            // "amount=10.0&label=Black & White")
            // but should be OK for early use cases.
            int queryParamIndex = rawURI.indexOf("?");
            if (queryParamIndex > 0 && !rawURI.contains("%")) {
                // Possibly encoded but more likely not
                String encodedQueryParams = URLEncoder.encode(rawURI.substring(queryParamIndex + 1), "UTF-8");
                rawURI = rawURI.substring(0, queryParamIndex) + "?" + encodedQueryParams;
                rawURI = rawURI.replaceAll("%3D", "=");
                rawURI = rawURI.replaceAll("%26", "&");
            }
            final URI uri;
            log.debug("Working with '{}' as a Bitcoin URI", rawURI);
            // Construct an OpenURIEvent to simulate receiving this from a
            // listener
            uri = new URI(rawURI);
            GenericOpenURIEvent event = new GenericOpenURIEvent() {
                @Override
                public URI getURI() {
                    return uri;
                }
            };
            controller.displayView(controller.getCurrentView());
            // Call the event which will attempt validation against the
            // Bitcoin URI specification.
            coreController.onOpenURIEvent(event);
        } catch (URISyntaxException e) {
            log.error("URI is malformed. Received: '{}'", rawURI);
        } catch (UnsupportedEncodingException e) {
            log.error("UTF=8 is not supported on this platform");
        }
    }
}
