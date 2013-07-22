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

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.BackupManager;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletInfoData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.WalletFileFilter;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} creates a new wallet.
 */
public class CreateWalletSubmitAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(CreateWalletSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;

    private Font adjustedFont;

    /**
     * Creates a new {@link CreateWalletSubmitAction}.
     */
    public CreateWalletSubmitAction(BitcoinController bitcoinController, ImageIcon icon, MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("createNewWalletAction.text"), icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("createNewWalletAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createNewWalletAction.text"));
    }

    /**
     * Create new wallet.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (mainFrame != null) {
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }
        setEnabled(false);

        try {
            // Create a file save dialog.

            JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setLocale(controller.getLocaliser().getLocale());
            fileChooser.setDialogTitle(controller.getLocaliser().getString("createNewWalletAction.tooltip"));

            adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
            if (adjustedFont != null) {
                setFileChooserFont(new Container[] {fileChooser});
            }
            fileChooser.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            if (this.bitcoinController.getModel().getActiveWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(this.bitcoinController.getModel().getActiveWalletFilename()));
            }
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            fileChooser.setFileFilter(new WalletFileFilter(controller));
            String defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile() + File.separator
                    + controller.getLocaliser().getString("saveWalletAsView.untitled") + "." + BitcoinModel.WALLET_FILE_EXTENSION;
            fileChooser.setSelectedFile(new File(defaultFileName));

            fileChooser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            int returnVal = fileChooser.showSaveDialog(mainFrame);

            String newWalletFilename = null;
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = fileChooser.getSelectedFile();
                if (file != null) {
                    newWalletFilename = file.getAbsolutePath();
                    createNewWallet(newWalletFilename);
                }
            }
        } finally {
            setEnabled(true);
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
        }
    }

    public void createNewWallet(String newWalletFilename) {
        String message;
        if (new File(newWalletFilename).isDirectory()) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletFileIsADirectory",
                    new Object[] { newWalletFilename });
            log.debug(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            return;
        }

        // If the filename has no extension, put on the wallet extension.
        if (!newWalletFilename.contains(".")) {
             newWalletFilename = newWalletFilename + "." + BitcoinModel.WALLET_FILE_EXTENSION;
        }

        File newWalletFile = new File(newWalletFilename);
        
        boolean theWalletWasNotOpenedSuccessfully = false;

        try {
            // If file exists, load the existing wallet.
            if (newWalletFile.exists()) {
                WalletData perWalletModelData = this.bitcoinController.getFileHandler().loadFromFile(newWalletFile);
                if (perWalletModelData != null) {
                    // Use the existing wallet.
                    this.bitcoinController.addWalletFromFilename(newWalletFile.getAbsolutePath());
                    this.bitcoinController.getModel().setActiveWalletByFilename(newWalletFilename);
                    controller.getModel().setUserPreference(BitcoinModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");
                    controller.fireRecreateAllViews(true);
                    controller.fireDataChangedUpdateNow();
                }
            } else {
                // Create a new wallet - protobuf.2 initially for backwards compatibility.
                Wallet newWallet = new Wallet(this.bitcoinController.getModel().getNetworkParameters());

                ECKey newKey = new ECKey();
                newWallet.addKey(newKey);
                WalletData perWalletModelData = new WalletData();
                perWalletModelData.setWalletInfo(new WalletInfoData(newWalletFilename, newWallet, MultiBitWalletVersion.PROTOBUF));
                perWalletModelData.setWallet(newWallet);
                perWalletModelData.setWalletFilename(newWalletFilename);
                perWalletModelData.setWalletDescription(controller.getLocaliser().getString(
                        "createNewWalletSubmitAction.defaultDescription"));
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
            }
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