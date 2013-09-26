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
package org.multibit.viewsystem.swing;

/**
 * L2FProd.com Common Components 7.3 License.
 *
 * Copyright 2005-2007 L2FProd.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.google.bitcoin.core.*;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.hardwarewallet.HardwareWallet;
import org.multibit.hardwarewallet.HardwareWalletManager;
import org.multibit.message.Message;
import org.multibit.message.MessageListener;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.model.core.StatusEnum;
import org.multibit.network.MultiBitCheckpointManager;
import org.multibit.network.ReplayManager;
import org.multibit.network.ReplayTask;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.action.MultiBitAction;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanel;
import org.multibit.viewsystem.swing.view.walletlist.WalletListPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.co.bsol.trezorj.core.Trezor;
import uk.co.bsol.trezorj.core.protobuf.TrezorMessage;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.List;
import java.util.Timer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * StatusBar. <BR>
 * A status bar is made of multiple zones. A zone can be any JComponent.
 * <p/>
 * In MultiBit the StatusBar is responsible for :
 * <p/>
 * 1) Online/ Connecting/ Error status label
 * 2) Status messages - these are cleared after a period of time
 * 3) Synchronisation progress bar
 */
public class StatusBar extends JPanel implements MessageListener {

    private static final Logger log = LoggerFactory.getLogger(StatusBar.class);

    private static final long serialVersionUID = 7824115980324911080L;

    private static final int A_SMALL_NUMBER_OF_PIXELS = 100;
    private static final int A_LARGE_NUMBER_OF_PIXELS = 1000000;
    private static final int STATUSBAR_HEIGHT = 30;

    private static final double TOLERANCE = 0.0000001;

    public static final int ONLINE_LABEL_WIDTH_DELTA = 12;
    public static final int ONLINE_LABEL_HEIGHT_DELTA = 8;

    private JLabel onlineLabel;
    final private MultiBitButton statusLabel;
    private StatusEnum statusEnum;

    public static final long TIMER_REPEAT_TIME = 5000; // millisecond
    public static final int NUMBER_OF_REPEATS = 12;

    private Timer statusClearTimer;
    static boolean clearAutomatically = true;

    private HashMap<String, Component> idToZones;
    private Border zoneBorder;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private MultiBitFrame mainFrame;

    private JProgressBar syncProgressBar;

    private SimpleDateFormat dateFormatter;

    /**
     * Construct a new StatusBar.
     */
    public StatusBar(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;

        this.mainFrame = mainFrame;

        setLayout(LookAndFeelTweaks.createHorizontalPercentLayout(controller.getLocaliser().getLocale()));
        idToZones = new HashMap<String, Component>();
        setZoneBorder(BorderFactory.createEmptyBorder());
        setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
        setOpaque(true);
        setBorder(BorderFactory.createMatteBorder(2, 0, 2, 0, ColorAndFontConstants.MID_BACKGROUND_COLOR));

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        final BitcoinController finalController = this.bitcoinController;

        dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        onlineLabel = new JLabel("");
        onlineLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        onlineLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        onlineLabel.setOpaque(true);

        onlineLabel.setHorizontalAlignment(SwingConstants.CENTER);
        onlineLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        onlineLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent arg0) {
                int blockHeight = -1;
                if (finalController.getMultiBitService() != null) {
                    if (finalController.getMultiBitService().getChain() != null) {
                        if (finalController.getMultiBitService().getChain().getChainHead() != null) {
                            blockHeight = finalController.getMultiBitService().getChain().getChainHead().getHeight();
                            Block blockHeader = finalController.getMultiBitService().getChain().getChainHead().getHeader();
                            Date blockTime = blockHeader.getTime();
                            int numberOfPeers = 0;
                            if (finalController.getMultiBitService().getPeerGroup() != null && finalController.getMultiBitService().getPeerGroup().getConnectedPeers() != null) {
                                numberOfPeers = finalController.getMultiBitService().getPeerGroup().getConnectedPeers().size();
                            }
                            onlineLabel.setToolTipText(HelpContentsPanel.createMultilineTooltipText(new String[]{
                                finalController.getLocaliser().getString("multiBitFrame.numberOfBlocks",
                                    new Object[]{blockHeight}),
                                finalController.getLocaliser().getString("multiBitFrame.blockDate",
                                    new Object[]{dateFormatter.format(blockTime)}),
                                finalController.getLocaliser().getString("multiBitFrame.connectedTo",
                                    new Object[]{numberOfPeers})}));
                        }
                    }
                }
            }
        });

        statusLabel = new MultiBitButton("");
        statusLabel.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
        statusLabel.setOpaque(true);
        statusLabel.setBorderPainted(false);
        statusLabel.setForeground(Color.BLACK);
        //statusLabel.setBorder(BorderFactory.createLineBorder(Color.CYAN));
        statusLabel.setFocusPainted(false);

        statusLabel.setContentAreaFilled(false);
        statusLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        // show messages action
        MultiBitAction showMessagesAction = new MultiBitAction(controller, null, null,
            "messagesPanel.title", "messagesPanel.mnemonic", org.multibit.viewsystem.View.MESSAGES_VIEW);
        statusLabel.setAction(showMessagesAction);
        statusLabel.setHorizontalAlignment(JButton.LEADING);
        String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[]{
            controller.getLocaliser().getString("multiBitFrame.statusBar.tooltip1"), "\n",
            controller.getLocaliser().getString("multiBitFrame.statusBar.tooltip2")});
        statusLabel.setToolTipText(tooltipText);

        int onlineWidth = Math.max(Math.max(
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                controller.getLocaliser().getString("multiBitFrame.onlineText")),
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                controller.getLocaliser().getString("multiBitFrame.offlineText"))),
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                controller.getLocaliser().getString("multiBitFrame.errorText"))
        )
            + ONLINE_LABEL_WIDTH_DELTA;

        int onlineHeight = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + ONLINE_LABEL_HEIGHT_DELTA;

        onlineLabel.setPreferredSize(new Dimension(onlineWidth, onlineHeight));
        int statusBarHeight = Math.max(STATUSBAR_HEIGHT, onlineHeight);
        setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, statusBarHeight));
        setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, statusBarHeight));

        syncProgressBar = new JProgressBar(0, 100);
        syncProgressBar.setValue(0);
        syncProgressBar.setStringPainted(false);
        syncProgressBar.setVisible(false);
        syncProgressBar.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
        syncProgressBar.setOpaque(true);
        syncProgressBar.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        JPanel filler = new JPanel();
        filler.setOpaque(true);
        filler.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);

        JButton connectTrezorTestButton = new JButton("Connect");
        connectTrezorTestButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ExecutorService executorService = Executors.newSingleThreadExecutor();

                executorService.submit(new Runnable() {
                    @Override
                    public void run() {
                        connectTrezorTest();
                    }
                });
            }
        }
        );
        JButton disconnectTrezorTestButton = new JButton("Ping");
        disconnectTrezorTestButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ExecutorService executorService = Executors.newSingleThreadExecutor();

                executorService.submit(new Runnable() {
                    @Override
                    public void run() {
                        disconnectTrezorTest();
                    }
                });
            }
        }
        );

        addZone("online", onlineLabel, "" + onlineWidth, "left");
        addZone("progressBar", syncProgressBar, "" + 200, "left");
        addZone("network", statusLabel, "*", "");
        addZone("connectTrezorTest", connectTrezorTestButton, "100", "");
        addZone("disconnectTrezorTest", disconnectTrezorTestButton, "100", "");
        addZone("filler2", filler, "0", "right");

        statusClearTimer = new java.util.Timer();
        statusClearTimer.schedule(new StatusClearTask(statusLabel), TIMER_REPEAT_TIME, TIMER_REPEAT_TIME);
    }

    /**
     * Test code that simulates a trezor connection and initialisation.
     */
    private void connectTrezorTest() {
        try {
            HardwareWalletManager hardwareWalletManager = HardwareWalletManager.INSTANCE;

            // Create a HardwareWallet object. This also wires up the HardwareWalletManager to listen for trezor events.
            HardwareWallet hardwareWallet = hardwareWalletManager.createMockTrezor();
            Trezor client = hardwareWallet.getTrezorClient();

            // Connect up the mockTrezor - this is the physical equivalent of plugging in a Trezor.
            client.connect();

            Thread.sleep(2000);

            // Initialise the device.
            MessageManager.INSTANCE.addMessage(new Message("Initialising trezor device ..."));
            hardwareWallet.initialise();

            // After some period of time the wallet should be initialised.
            // Give it 2 seconds (should really listen for hasInitalised event on HardwareWalletListener
            Thread.sleep(2000);
            MessageManager.INSTANCE.addMessage(new Message("Trezor device initialised successfully. Serial id = " + hardwareWallet.getSerialId()));

            if (hardwareWallet.getSerialId() != null) {
                // Work out the name of the trezor wallet from the serial id
                // TODO probably needs to also use the MPK and then hash it for privacy.
                // Wallet filename is <user data directory>/"trezor-" + serialId + ".wallet"
                // TODO think about name collisions
                String walletFilename = bitcoinController.getApplicationDataDirectoryLocator().getApplicationDataDirectory() + File.separator +
                        "trezor-" + hardwareWallet.getSerialId() + ".wallet";
                String walletDescription = "trezor-" + hardwareWallet.getSerialId();
                File walletFile = new File(walletFilename);
                if (walletFile.exists()) {
                    // If the wallet already exists, open it.
                    boolean walletIsAlreadyOpen = false;
                    if (controller != null && controller.getModel() != null) {
                        List<WalletData> perWalletDataModels = this.bitcoinController.getModel().getPerWalletModelDataList();
                        if (perWalletDataModels != null) {
                            Iterator<WalletData> iterator = perWalletDataModels.iterator();
                            if (iterator != null) {
                                while(iterator.hasNext()) {
                                    WalletData perWalletModelData = iterator.next();
                                    if (perWalletModelData != null && perWalletModelData.getWalletFilename() != null) {
                                        if (perWalletModelData.getWalletFilename().equals(walletFilename)) {
                                            bitcoinController.getModel().setActiveWalletByFilename(walletFilename);
                                            bitcoinController.getModel().getActivePerWalletModelData().setHardwareWallet(hardwareWallet);
                                            controller.fireDataChangedUpdateNow();
                                            walletIsAlreadyOpen = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (!walletIsAlreadyOpen)  {
                        Message openMessage = new Message(bitcoinController.getLocaliser().getString("multiBit.openingWallet", new Object[]{walletFilename}));
                        openMessage.setShowInStatusBar(false);
                        MessageManager.INSTANCE.addMessage(openMessage);
                        openWalletInBackground(walletFilename, hardwareWallet);
                    }
                } else {
                    // Create a new wallet.
                    // TODO this should be an HD watch only wallet initialised with the trezor MPK.
                    createNewWallet(walletFilename, walletDescription, hardwareWallet);
                }
            }

        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Test code that simulates a trezor disconnection.
     */
    private void disconnectTrezorTest() {
            HardwareWalletManager hardwareWalletManager = HardwareWalletManager.INSTANCE;

            HardwareWallet hardwareWallet = hardwareWalletManager.getHardwareWallet();

            if (hardwareWallet != null && hardwareWallet.getTrezorClient() != null) {
                // Close the Trezor.
                // This is the physical equivalent of removing a Trezor device.
                hardwareWallet.getTrezorClient().sendMessage(TrezorMessage.Ping.getDefaultInstance());

                //Thread.sleep(2000);

                // Destroy the trezor device
                //hardwareWalletManager.destroyMockTrezor();
            }
    }

    /**
     * Open a wallet in a background Swing worker thread.
     * @param selectedWalletFilename Filename of wallet to open
     */
    private void openWalletInBackground(String selectedWalletFilename, final HardwareWallet hardwareWallet) {
        final String selectedWalletFilenameFinal = selectedWalletFilename;

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {

            private String message = null;

            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    log.debug("Opening wallet '" + selectedWalletFilenameFinal + "'.");
                    WalletData perWalletModelData = bitcoinController.addWalletFromFilename(selectedWalletFilenameFinal);
                    perWalletModelData.setHardwareWallet(hardwareWallet);

                    log.debug("Setting active wallet for file '" + selectedWalletFilenameFinal + "'.");
                    bitcoinController.getModel().setActiveWalletByFilename(selectedWalletFilenameFinal);

                    // Save the user properties to disk.
                    log.debug("Writing user preferences. . .");
                    FileHandler.writeUserPreferences(bitcoinController);
                    log.debug("User preferences with new wallet written successfully");

                    // Backup the wallet and wallet info.
                    BackupManager.INSTANCE.backupPerWalletModelData(bitcoinController.getFileHandler(), perWalletModelData);

                    message = controller.getLocaliser().getString("multiBit.openingWalletIsDone", new Object[]{selectedWalletFilenameFinal});

                    return Boolean.TRUE;
                } catch (WalletLoadException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (WalletVersionException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (IOException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }  catch (WalletSaveException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (Exception e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }
            }

            @Override
            protected void done() {
                try {
                    Boolean wasSuccessful = get();
                    if (wasSuccessful) {
                        log.debug(message);
                        Message messageMessage = new Message(message);
                        messageMessage.setShowInStatusBar(false);
                        MessageManager.INSTANCE.addMessage(messageMessage);

                        // Work out the late date/ block the wallet saw to see if it needs syncing.
                        WalletData perWalletModelData = bitcoinController.getModel().getActivePerWalletModelData();
                        Wallet wallet = perWalletModelData.getWallet();
                        int lastBlockSeenHeight = wallet.getLastBlockSeenHeight();
                        log.debug("For wallet '" + perWalletModelData.getWalletFilename() + " the lastBlockSeenHeight was " + lastBlockSeenHeight);

                        // If there was no lastBlockSeenHeight, find the latest confirmed transaction height.
                        if (lastBlockSeenHeight <= 0) {
                            Set<Transaction> transactions = perWalletModelData.getWallet().getTransactions(true);
                            if (transactions != null) {
                                for (Transaction transaction : transactions) {
                                    TransactionConfidence confidence = transaction.getConfidence();
                                    if (confidence != null) {
                                        if (TransactionConfidence.ConfidenceType.BUILDING.equals(confidence.getConfidenceType())) {
                                            lastBlockSeenHeight = Math.max(lastBlockSeenHeight, confidence.getAppearedAtChainHeight());
                                        }
                                    }
                                }
                            }
                        }
                        log.debug("For wallet '" + perWalletModelData.getWalletFilename() + ", after transactions, the lastBlockSeenHeight was " + lastBlockSeenHeight);

                        int currentChainHeight = -1;
                        if (bitcoinController.getMultiBitService().getChain() != null) {
                            if (bitcoinController.getMultiBitService().getChain().getChainHead() != null) {
                                currentChainHeight = bitcoinController.getMultiBitService().getChain().getChainHead().getHeight();
                            }
                        }
                        log.debug("The current chain height is " + currentChainHeight);

                        boolean needToSync = false;
                        long requiredSyncTimeInSeconds = -1;

                        // Check if we have both the lastBlockSeenHeight and the currentChainHeight.
                        if (lastBlockSeenHeight > 0 && currentChainHeight > 0) {
                            if (lastBlockSeenHeight >= currentChainHeight) {
                                // Wallet is at or ahead of current chain - but check there isnt a replay at the moment.
                                // If there is, use the actual last chain height to check.
                                // (The user might have opened a wallet whilst a replay was occurring).
                                if (ReplayManager.INSTANCE.getCurrentReplayTask() != null) {
                                    int actualLastChainHeight = ReplayManager.INSTANCE.getActualLastChainHeight();
                                    if (lastBlockSeenHeight < actualLastChainHeight) {
                                        needToSync = true;
                                    }
                                }
                            } else {
                                // Wallet is behind the current chain - need to sync.
                                needToSync = true;
                            }
                        } else {
                            // If we still dont have a sync date/height, use the key birth date of the wallet
                            // and sync from that. This might be expensive, but should only be used for empty
                            // wallets.
                            requiredSyncTimeInSeconds = wallet.getEarliestKeyCreationTime();
                            needToSync = true;
                            log.debug("requiredSyncTimeInSeconds worked out from earliestKeyCreationTime = " + requiredSyncTimeInSeconds);
                        }

                        log.debug("needToSync = " + needToSync);

                        if (needToSync) {
                            StoredBlock syncFromStoredBlock = null;

                            MultiBitCheckpointManager checkpointManager = bitcoinController.getMultiBitService().getCheckpointManager();
                            if (checkpointManager != null) {
                                if (lastBlockSeenHeight > 0) {
                                    syncFromStoredBlock = checkpointManager.getCheckpointBeforeOrAtHeight(lastBlockSeenHeight);
                                } else {
                                    if (requiredSyncTimeInSeconds >= 0) {
                                        syncFromStoredBlock = checkpointManager.getCheckpointBefore(requiredSyncTimeInSeconds);
                                    }
                                }
                            }
                            log.debug("syncFromStoredBlock =" + syncFromStoredBlock);

                            // Initialise the message in the singleWalletPanel.
                            if (mainFrame != null) {
                                WalletListPanel walletListPanel = mainFrame.getWalletsView();
                                if (walletListPanel != null) {
                                    SingleWalletPanel singleWalletPanel = walletListPanel.findWalletPanelByFilename(selectedWalletFilenameFinal);
                                    if (singleWalletPanel != null) {
                                        singleWalletPanel.setSyncMessage(controller.getLocaliser().getString("multiBitDownloadListener.downloadingTextShort"), Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
                                    }
                                }
                            }
                            List<WalletData> perWalletModelDataList = new ArrayList<WalletData>();
                            perWalletModelDataList.add(perWalletModelData);
                            ReplayTask replayTask;
                            if (syncFromStoredBlock == null) {
                                // Sync from genesis block.
                                replayTask = new ReplayTask(perWalletModelDataList, null, 0);
                            } else {
                                Date syncDate = null;
                                if (syncFromStoredBlock.getHeader() != null) {
                                    syncDate = new Date(syncFromStoredBlock.getHeader().getTimeSeconds() * 1000);
                                }
                                replayTask = new ReplayTask(perWalletModelDataList, syncDate, syncFromStoredBlock.getHeight());
                            }
                            ReplayManager.INSTANCE.offerReplayTask(replayTask);
                        }
                        controller.fireRecreateAllViews(true);
                    } else {
                        log.error(message);
                        MessageManager.INSTANCE.addMessage(new Message(message));
                        WalletData loopData = bitcoinController.getModel().getPerWalletModelDataByWalletFilename(selectedWalletFilenameFinal);
                        if (loopData != null) {
                            // Clear the backup wallet filename - this prevents it being automatically overwritten.
                            if (loopData.getWalletInfo() != null) {
                                loopData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, "");
                            }
                        }
                    }
                } catch (Exception e) {
                    // Not really used but caught so that SwingWorker shuts down cleanly.
                    log.error(e.getClass() + " " + e.getMessage());
                } finally {
                    setEnabled(true);
                    if (mainFrame != null) {
                        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    }
                }
            }
        };
        log.debug("Executing open of wallet '" + selectedWalletFilenameFinal + "'.");
        worker.execute();
    }

    public void createNewWallet(String newWalletFilename, String walletDescription, HardwareWallet hardwareWallet) {
        String message;
        if (new File(newWalletFilename).isDirectory()) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletFileIsADirectory",
                    new Object[] { newWalletFilename });
            log.debug(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            return;
        }

        File newWalletFile = new File(newWalletFilename);

        boolean theWalletWasNotOpenedSuccessfully = false;

        try {
            // Create a new wallet - protobuf.2 initially for backwards compatibility.
            Wallet newWallet = new Wallet(this.bitcoinController.getModel().getNetworkParameters());

            ECKey newKey = new ECKey();
            newWallet.addKey(newKey);
            WalletData perWalletModelData = new WalletData();
            perWalletModelData.setWalletInfo(new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF));
            perWalletModelData.setWallet(newWallet);
            perWalletModelData.setWalletFilename(newWalletFilename);
            perWalletModelData.setWalletDescription(walletDescription);
            perWalletModelData.setHardwareWallet(hardwareWallet);
            this.bitcoinController.getFileHandler().savePerWalletModelData(perWalletModelData, true);

            // Start using the new file as the wallet.
            this.bitcoinController.addWalletFromFilename(newWalletFile.getAbsolutePath());
            this.bitcoinController.getModel().setActiveWalletByFilename(newWalletFilename);
            controller.getModel().setUserPreference(BitcoinModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");

            // Save the user properties to disk.
            FileHandler.writeUserPreferences(this.bitcoinController);
            log.debug("User preferences with new wallet written successfully");

            // Backup the wallet and wallet info.
            BackupManager.INSTANCE.backupPerWalletModelData(bitcoinController.getFileHandler(), perWalletModelData);

            controller.fireRecreateAllViews(true);
            controller.fireDataChangedUpdateNow();
        } catch (WalletLoadException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated",
                    new Object[] { newWalletFilename, e.getMessage() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            theWalletWasNotOpenedSuccessfully = true;
        } catch (WalletSaveException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated",
                    new Object[] { newWalletFilename, e.getMessage() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            theWalletWasNotOpenedSuccessfully = true;
        } catch (WalletVersionException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated",
                    new Object[] { newWalletFilename, e.getMessage() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            theWalletWasNotOpenedSuccessfully = true;
        } catch (IOException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated",
                    new Object[] { newWalletFilename, e.getMessage() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            theWalletWasNotOpenedSuccessfully = true;
        }

        if (theWalletWasNotOpenedSuccessfully) {
            WalletData loopData = this.bitcoinController.getModel().getPerWalletModelDataByWalletFilename(newWalletFilename);
            if (loopData != null) {
                // Clear the backup wallet filename - this prevents it being automatically overwritten.
                if (loopData.getWalletInfo() != null) {
                    loopData.getWalletInfo().put(BitcoinModel.WALLET_BACKUP_FILE, "");
                }
            }
        }
    }

    /**
     * initialise the statusbar;
     */
    public void initialise() {
        updateOnlineStatusText(StatusEnum.CONNECTING);
        updateStatusLabel("", true);
    }

    /**
     * refresh online status text with existing value
     */
    public void refreshOnlineStatusText() {
        updateOnlineStatusText(statusEnum);
    }

    /**
     * Update online status text with new value.
     *
     * @param finalStatusEnum
     */
    public void updateOnlineStatusText(final StatusEnum finalStatusEnum) {
        if (EventQueue.isDispatchThread()) {
            updateOnlineStatusTextOnSwingThread(finalStatusEnum);
        } else {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    updateOnlineStatusTextOnSwingThread(finalStatusEnum);
                }
            });
        }
    }

    /**
     * Update online status text with new value.
     *
     * @param finalStatusEnum
     */
    public void updateOnlineStatusTextOnSwingThread(final StatusEnum finalStatusEnum) {
        this.statusEnum = finalStatusEnum;
        String onlineStatus = controller.getLocaliser().getString(finalStatusEnum.getLocalisationKey());
        if (finalStatusEnum == StatusEnum.ONLINE) {
            onlineLabel.setForeground(new Color(0, 100, 0));
            if (mainFrame != null) {
                BlinkLabel estimatedBalanceBTCLabel = mainFrame.getEstimatedBalanceBTCLabel();
                if (estimatedBalanceBTCLabel != null) {
                    estimatedBalanceBTCLabel.setBlinkEnabled(true);
                }
                BlinkLabel estimatedBalanceFiatLabel = mainFrame.getEstimatedBalanceFiatLabel();
                if (estimatedBalanceFiatLabel != null) {
                    estimatedBalanceFiatLabel.setBlinkEnabled(true);
                }
            }
        } else {
            onlineLabel.setForeground(new Color(180, 0, 0));
        }
        onlineLabel.setText(onlineStatus);
        if (finalStatusEnum == StatusEnum.ERROR) {
            // Set tooltip to look at Messages view
            String toolTip = HelpContentsPanel.createMultilineTooltipText(new String[]{
                controller.getLocaliser().getString("multiBitFrame.statusBar.error1"),
                controller.getLocaliser().getString("multiBitFrame.statusBar.error2")});
            onlineLabel.setToolTipText(toolTip);
        }
    }

    synchronized private void startSync() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                syncProgressBar.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
                syncProgressBar.setValue(0);
                syncProgressBar.setVisible(true);
            }
        });
    }

    synchronized private void finishSync() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                syncProgressBar.setValue(100);
                syncProgressBar.setVisible(false);
            }
        });
    }

    synchronized private void updateSync(final int percent, final String syncMessage) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                syncProgressBar.setValue(percent);
                syncProgressBar.setToolTipText(HelpContentsPanel.createTooltipText(syncMessage));

                // when a language changes the progress bar needs to be made visible
                syncProgressBar.setVisible(true);
            }
        });
    }

    @Override
    public void newMessageReceived(final Message newMessage) {
        if (newMessage == null || !newMessage.isShowInStatusBar()) {
            return;
        }

        if (newMessage.getPercentComplete() == Message.NOT_RELEVANT_PERCENTAGE_COMPLETE) {
            updateStatusLabel(newMessage.getText(), newMessage.isClearAutomatically());
        } else {
            if (Math.abs(newMessage.getPercentComplete() - 0) < TOLERANCE) {
                startSync();
            }
            updateSync((int) newMessage.getPercentComplete(), newMessage.getText());

            if (Math.abs(newMessage.getPercentComplete() - 100) < TOLERANCE) {
                finishSync();
            }
        }
    }

    private void updateStatusLabel(final String newStatusLabel, Boolean clearAutomatically) {
        StatusBar.clearAutomatically = clearAutomatically;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (statusLabel != null) {
                    log.debug("StatusBar label = '" + newStatusLabel + "'");
                    statusLabel.setText(newStatusLabel);
                }
            }
        });
    }

    private void setZoneBorder(Border border) {
        zoneBorder = border;
    }

    /**
     * Adds a new zone in the StatusBar
     *
     * @param id
     * @param zone
     * @param constraints one of the constraint support by the
     *                    com.l2fprod.common.swing.PercentLayout
     */
    private void addZone(String id, Component zone, String constraints, String tweak) {
        // is there already a zone with this id?
        Component previousZone = getZone(id);
        if (previousZone != null) {
            remove(previousZone);
            idToZones.remove(id);
        }

        if (zone instanceof JComponent) {
            JComponent jc = (JComponent) zone;
            jc.setOpaque(true);
            //jc.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            if (jc.getBorder() == null || jc.getBorder() instanceof UIResource) {
                if (jc instanceof JLabel) {
                    if ("left".equals(tweak)) {
                        Border border = new CompoundBorder(BorderFactory.createMatteBorder(0, 3, 0, 2, ColorAndFontConstants.BACKGROUND_COLOR), BorderFactory
                                .createLineBorder(Color.lightGray));
                        jc.setBorder(border);
                    } else {
                        if ("right".equals(tweak)) {
                            jc.setBorder(new CompoundBorder(zoneBorder, new EmptyBorder(0, 2, 0, 1)));
                        } else {
                            jc.setBorder(new CompoundBorder(zoneBorder, new EmptyBorder(0, 2, 0, 2)));
                        }
                    }
                    ((JLabel) jc).setText(" ");
                } else {
                    if (!(jc instanceof JPanel)) {
                        jc.setBorder(zoneBorder);
                    }
                }
            }
        }

        add(zone, constraints);
        idToZones.put(id, zone);
    }

    public Component getZone(String id) {
        return idToZones.get(id);
    }
}

/**
 * PercentLayout. <BR>
 * Constraint based layout which allow the space to be splitted using
 * percentages. The following are allowed when adding components to container:
 * <ul>
 * <li>container.add(component); <br>
 * in this case, the component will be sized to its preferred size
 * <li>container.add(component, "100"); <br>
 * in this case, the component will have a width (or height) of 100
 * <li>container.add(component, "25%"); <br>
 * in this case, the component will have a width (or height) of 25 % of the
 * container width (or height) <br>
 * <li>container.add(component, "*"); <br>
 * in this case, the component will take the remaining space. if several
 * components use the "*" constraint the space will be divided among the
 * components.
 * </ul>
 *
 * @javabean.class name="PercentLayout"
 * shortDescription="A layout supports constraints expressed in percent."
 */
class PercentLayout implements LayoutManager2 {

    /**
     * Useful constant to layout the components horizontally (from top to
     * bottom).
     */
    public final static int HORIZONTAL = 0;

    /**
     * Useful constant to layout the components vertically (from left to right).
     */
    public final static int VERTICAL = 1;

    static class Constraint {
        protected Object value;

        private Constraint(Object value) {
            this.value = value;
        }
    }

    static class NumberConstraint extends Constraint {
        public NumberConstraint(int d) {
            this(Integer.valueOf(d));
        }

        public NumberConstraint(Integer d) {
            super(d);
        }

        public int intValue() {
            return (Integer) value;
        }
    }

    static class PercentConstraint extends Constraint {
        public PercentConstraint(float d) {
            super(d);
        }

        public float floatValue() {
            return (Float) value;
        }
    }

    private final static Constraint REMAINING_SPACE = new Constraint("*");

    private final static Constraint PREFERRED_SIZE = new Constraint("");

    private int orientation;
    private int gap;

    private Locale locale;

    // Consider using HashMap
    private Hashtable<Component, Constraint> m_ComponentToConstraint;


    public PercentLayout(int orientation, int gap, Locale locale) {
        setOrientation(orientation);
        this.gap = gap;
        this.locale = locale;

        m_ComponentToConstraint = new Hashtable<Component, Constraint>();
    }

    public void setGap(int gap) {
        this.gap = gap;
    }

    /**
     * @javabean.property bound="true" preferred="true"
     */
    public int getGap() {
        return gap;
    }

    public void setOrientation(int orientation) {
        if (orientation != HORIZONTAL && orientation != VERTICAL) {
            throw new IllegalArgumentException("Orientation must be one of HORIZONTAL or VERTICAL");
        }
        this.orientation = orientation;
    }

    /**
     * @javabean.property bound="true" preferred="true"
     */
    public int getOrientation() {
        return orientation;
    }

    public void setConstraint(Component component, Object constraints) {
        if (constraints instanceof Constraint) {
            m_ComponentToConstraint.put(component, (Constraint) constraints);
        } else if (constraints instanceof Number) {
            setConstraint(component, new NumberConstraint(((Number) constraints).intValue()));
        } else if ("*".equals(constraints)) {
            setConstraint(component, REMAINING_SPACE);
        } else if ("".equals(constraints)) {
            setConstraint(component, PREFERRED_SIZE);
        } else if (constraints instanceof String) {
            String s = (String) constraints;
            if (s.endsWith("%")) {
                float value = Float.valueOf(s.substring(0, s.length() - 1)) / 100;
                if (value > 1 || value < 0) {
                    throw new IllegalArgumentException("percent value must be >= 0 and <= 100");
                }
                setConstraint(component, new PercentConstraint(value));
            } else {
                setConstraint(component, new NumberConstraint(Integer.valueOf(s)));
            }
        } else if (constraints == null) {
            // null constraint means preferred size
            setConstraint(component, PREFERRED_SIZE);
        } else {
            throw new IllegalArgumentException("Invalid Constraint");
        }
    }

    @Override
    public void addLayoutComponent(Component component, Object constraints) {
        setConstraint(component, constraints);
    }

    /**
     * Returns the alignment along the x axis. This specifies how the component
     * would like to be aligned relative to other components. The value should
     * be a number between 0 and 1 where 0 represents alignment along the
     * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
     * etc.
     */
    @Override
    public float getLayoutAlignmentX(Container target) {
        return 1.0f / 2.0f;
    }

    /**
     * Returns the alignment along the y axis. This specifies how the component
     * would like to be aligned relative to other components. The value should
     * be a number between 0 and 1 where 0 represents alignment along the
     * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
     * etc.
     */
    @Override
    public float getLayoutAlignmentY(Container target) {
        return 1.0f / 2.0f;
    }

    /**
     * Invalidates the layout, indicating that if the layout manager has cached
     * information it should be discarded.
     */
    @Override
    public void invalidateLayout(Container target) {
    }

    /**
     * Adds the specified component with the specified name to the layout.
     *
     * @param name the component name
     * @param comp the component to be added
     */
    @Override
    public void addLayoutComponent(String name, Component comp) {
    }

    /**
     * Removes the specified component from the layout.
     *
     * @param comp the component ot be removed
     */
    @Override
    public void removeLayoutComponent(Component comp) {
        m_ComponentToConstraint.remove(comp);
    }

    /**
     * Calculates the minimum size dimensions for the specified panel given the
     * components in the specified parent container.
     *
     * @param parent the component to be laid out
     * @see #preferredLayoutSize
     */
    @Override
    public Dimension minimumLayoutSize(Container parent) {
        return preferredLayoutSize(parent);
    }

    /**
     * Returns the maximum size of this component.
     *
     * @see java.awt.Component#getMinimumSize()
     * @see java.awt.Component#getPreferredSize()
     * @see java.awt.LayoutManager
     */
    @Override
    public Dimension maximumLayoutSize(Container parent) {
        return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    @Override
    public Dimension preferredLayoutSize(Container parent) {
        Component[] components = parent.getComponents();
        Insets insets = parent.getInsets();
        int width = 0;
        int height = 0;
        Dimension componentPreferredSize;
        boolean firstVisibleComponent = true;
        for (int i = 0, c = components.length; i < c; i++) {
            if (components[i].isVisible()) {
                componentPreferredSize = components[i].getPreferredSize();
                if (orientation == HORIZONTAL) {
                    height = Math.max(height, componentPreferredSize.height);
                    width += componentPreferredSize.width;
                    if (firstVisibleComponent) {
                        firstVisibleComponent = false;
                    } else {
                        width += gap;
                    }
                } else {
                    height += componentPreferredSize.height;
                    width = Math.max(width, componentPreferredSize.width);
                    if (firstVisibleComponent) {
                        firstVisibleComponent = false;
                    } else {
                        height += gap;
                    }
                }
            }
        }
        return new Dimension(width + insets.right + insets.left, height + insets.top + insets.bottom);
    }

    @Override
    public void layoutContainer(Container parent) {
        Insets insets = parent.getInsets();
        Dimension d = parent.getSize();

        // calculate the available sizes
        d.width = d.width - insets.left - insets.right;
        d.height = d.height - insets.top - insets.bottom;

        // pre-calculate the size of each components
        Component[] components = parent.getComponents();
        int[] sizes = new int[components.length];

        // calculate the available size
        int availableSize = (HORIZONTAL == orientation ? d.width : d.height) - (components.length - 1) * gap;

        // PENDING(fred): the following code iterates 4 times on the component
        // array, need to find something more efficient!

        // give priority to components who want to use their preferred size or
        // who
        // have a predefined size
        for (int i = 0, c = components.length; i < c; i++) {
            if (components[i].isVisible()) {
                Constraint constraint = m_ComponentToConstraint.get(components[i]);
                if (constraint == null || constraint == PREFERRED_SIZE) {
                    sizes[i] = (HORIZONTAL == orientation ? components[i].getPreferredSize().width : components[i]
                        .getPreferredSize().height);
                    availableSize -= sizes[i];
                } else if (constraint instanceof NumberConstraint) {
                    sizes[i] = ((NumberConstraint) constraint).intValue();
                    availableSize -= sizes[i];
                }
            }
        }

        // then work with the components who want a percentage of the remaining
        // space
        int remainingSize = availableSize;
        for (int i = 0, c = components.length; i < c; i++) {
            if (components[i].isVisible()) {
                Constraint constraint = m_ComponentToConstraint.get(components[i]);
                if (constraint instanceof PercentConstraint) {
                    sizes[i] = (int) (remainingSize * ((PercentConstraint) constraint).floatValue());
                    availableSize -= sizes[i];
                }
            }
        }

        // finally share the remaining space between the other components
        ArrayList<Integer> remaining = new ArrayList<Integer>();
        for (int i = 0, c = components.length; i < c; i++) {
            if (components[i].isVisible()) {
                Constraint constraint = m_ComponentToConstraint.get(components[i]);
                if (constraint == REMAINING_SPACE) {
                    remaining.add(i);
                    sizes[i] = 0;
                }
            }
        }

        if (remaining.size() > 0) {
            int rest = availableSize / remaining.size();
            for (Integer aRemaining : remaining) {
                sizes[aRemaining] = rest;
            }
        }

        // all calculations are done, apply the sizes
        int currentOffset = (HORIZONTAL == orientation ? insets.left : insets.top);
        if (!ComponentOrientation.getOrientation(locale).isLeftToRight()) {
            currentOffset = (HORIZONTAL == orientation ? d.width - insets.right : insets.top);
        }

        for (int i = 0, c = components.length; i < c; i++) {
            if (components[i].isVisible()) {
                if (HORIZONTAL == orientation) {
                    if (ComponentOrientation.getOrientation(locale).isLeftToRight()) {
                        components[i].setBounds(currentOffset, insets.top, sizes[i], d.height);
                    } else {
                        components[i].setBounds(currentOffset - sizes[i], insets.top, sizes[i], d.height);
                    }
                } else {
                    components[i].setBounds(insets.left, currentOffset, d.width, sizes[i]);
                }
                if (ComponentOrientation.getOrientation(locale).isLeftToRight()) {
                    currentOffset += gap + sizes[i];
                } else {
                    currentOffset = currentOffset - gap - sizes[i];
                }
            }
        }
    }

}

class StatusClearTask extends TimerTask {
    JButton statusLabel;
    private String previousStatusLabelText = null;
    private int previousLabelRepeats = 0;

    StatusClearTask(JButton statusLabel) {
        this.statusLabel = statusLabel;
    }

    @Override
    public void run() {
        String currentStatusLabelText = statusLabel.getText();

        boolean hasReset = false;

        if (previousLabelRepeats > StatusBar.NUMBER_OF_REPEATS) {
            if (currentStatusLabelText != null && !"".equals(currentStatusLabelText)
                && currentStatusLabelText.equals(previousStatusLabelText)) {
                if (StatusBar.clearAutomatically) {
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            // clear label
                            statusLabel.setText("");
                        }
                    });
                    previousStatusLabelText = "";
                    previousLabelRepeats = 0;
                    hasReset = true;
                }
            }
        }
        if (currentStatusLabelText != null && !currentStatusLabelText.equals(previousStatusLabelText)) {
            // different - reset
            previousStatusLabelText = currentStatusLabelText;
            previousLabelRepeats = 0;
        } else {
            if (currentStatusLabelText != null && currentStatusLabelText.equals(previousStatusLabelText)) {
                if (!hasReset) {
                    // label is the same as before
                    previousLabelRepeats++;
                }
            }
        }
    }
}

/**
 * LookAndFeelTweaks
 */
final class LookAndFeelTweaks {

    public final static Border PANEL_BORDER = BorderFactory.createMatteBorder(3, 3, 3, 3, ColorAndFontConstants.MID_BACKGROUND_COLOR);

    /**
     * Utility class should not have a public constructor
     */
    private LookAndFeelTweaks() {
    }

    public static PercentLayout createHorizontalPercentLayout(Locale locale) {
        return new PercentLayout(PercentLayout.HORIZONTAL, 4, locale);
    }

    public static void setBorder(JComponent component) {
        if (component instanceof JPanel) {
            component.setBorder(PANEL_BORDER);
        }
    }
}