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
package org.multibit.viewsystem.swing.action;

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.SwingWorker;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.bitcoin.PerWalletModelData;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.WalletFileFilter;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} opens a wallet from a file
 */
public class OpenWalletAction extends AbstractAction {

    public Logger log = LoggerFactory.getLogger(OpenWalletAction.class.getName());

    private static final long serialVersionUID = 1913592460523457705L;

    private final Controller controller;
    private final BitcoinController bitcoinController;

    private MultiBitFrame mainFrame;

    private JFileChooser fileChooser;
    
    private Font adjustedFont;

    private String selectedWalletFilename;

    /**
     * Creates a new {@link OpenWalletAction}.
     */
    public OpenWalletAction(BitcoinController bitcoinController, ImageIcon icon, MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("openWalletAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.mainFrame = mainFrame;
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("openWalletAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("openWalletAction.mnemonicKey"));
    }

    /**
     * show open file chooser and load wallet
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            if (fileChooser == null) {
                JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
                fileChooser = new JFileChooser();
                fileChooser.setLocale(controller.getLocaliser().getLocale());
                fileChooser.setDialogTitle(controller.getLocaliser().getString("openWalletAction.tooltip"));
                adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
                if (adjustedFont != null) {
                    setFileChooserFont(new Container[] {fileChooser});
                }
                fileChooser.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

                if (controller.getModel() != null && this.bitcoinController.getModel().getActiveWalletFilename() != null) {
                    fileChooser.setCurrentDirectory(new File(this.bitcoinController.getModel().getActiveWalletFilename()));
                }
                fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                fileChooser.setFileFilter(new WalletFileFilter(controller));
            }

            fileChooser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            int returnVal = fileChooser.showOpenDialog(mainFrame);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = fileChooser.getSelectedFile();
                if (file != null) {
                    if (!file.isDirectory()) {
                        selectedWalletFilename = file.getAbsolutePath();
                        Message openMessage = new Message(controller.getLocaliser().getString("multiBit.openingWallet", new Object[]{selectedWalletFilename}));
                        openMessage.setShowInStatusBar(false);
                        MessageManager.INSTANCE.addMessage(openMessage);
                        openWalletInBackground(selectedWalletFilename);
                    }
                } else {
                    selectedWalletFilename = null;
                    fileChooser = null;
                }
            } else {
                selectedWalletFilename = null;
                fileChooser = null;
            }
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    /**
     * open a wallet in a background Swing worker thread
     * @param selectedWalletFilename Filename of wallet to open
     */
    private void openWalletInBackground(String selectedWalletFilename) {
        final String selectedWalletFilenameFinal = selectedWalletFilename;

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            
            private String message = null;
            
            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    log.debug("Opening wallet '" + selectedWalletFilenameFinal + "' in background swing worker");

                    bitcoinController.addWalletFromFilename(selectedWalletFilenameFinal);
                    bitcoinController.getModel().setActiveWalletByFilename(selectedWalletFilenameFinal);

                    // save the user properties to disk
                    FileHandler.writeUserPreferences(bitcoinController);
                    log.debug("User preferences with new wallet written successfully");
 
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
                        
                        controller.fireRecreateAllViews(true);
                        controller.fireDataChangedUpdateNow();
                    } else {
                        log.error(message);
                        MessageManager.INSTANCE.addMessage(new Message(message));
                        PerWalletModelData loopData = bitcoinController.getModel().getPerWalletModelDataByWalletFilename(selectedWalletFilenameFinal);
                        if (loopData != null) {
                            // Clear the backup wallet filename - this prevents it being automatically overwritten.
                            if (loopData.getWalletInfo() != null) {
                                loopData.getWalletInfo().put(MultiBitModel.WALLET_BACKUP_FILE, "");
                            }
                        }
                    }
                } catch (Exception e) {
                    // not really used but caught so that SwingWorker shuts down cleanly
                    log.error(e.getClass() + " " + e.getMessage());
                } finally {
                    setEnabled(true);
                    if (mainFrame != null) {
                        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    }
                }
            }
        };
        log.debug("Executing open of wallet '" + selectedWalletFilenameFinal + "' in background swing worker");
        worker.execute();
    }
    
    private void setFileChooserFont(Component[] comp) {
        for (int x = 0; x < comp.length; x++) {
            if (comp[x] instanceof Container)
                setFileChooserFont(((Container) comp[x]).getComponents());
            try {
                comp[x].setFont(adjustedFont);
            } catch (Exception e) {
            }// do nothing
        }
    }
}