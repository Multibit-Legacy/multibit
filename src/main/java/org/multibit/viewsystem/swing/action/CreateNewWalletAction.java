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

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletInfoException;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.WalletFileFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * This {@link Action} creates a new wallet
 */
public class CreateNewWalletAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(CreateNewWalletAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link CreateNewWalletAction}.
     */
    public CreateNewWalletAction(MultiBitController controller, ImageIcon icon, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("createNewWalletAction.text"), icon);
        this.controller = controller;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createNewWalletAction.text"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createNewWalletAction.text"));
    }

    /**
     * create new wallet
     */
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            // create a file save dialog

            JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setLocale(controller.getLocaliser().getLocale());
            if (controller.getModel().getActiveWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(controller.getModel().getActiveWalletFilename()));
            }
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            fileChooser.setFileFilter(new WalletFileFilter(controller));
            String defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile() + File.separator
                    + controller.getLocaliser().getString("saveWalletAsView.untitled") + "." + MultiBitModel.WALLET_FILE_EXTENSION;
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
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    private void createNewWallet(String newWalletFilename) {
        String message;
        if (new File(newWalletFilename).isDirectory()) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletFileIsADirectory", new Object[]{newWalletFilename});
            log.debug(message);
            controller.updateStatusLabel(message);  
            return;
        }

        // if the filename has no extension, put on the wallet
        // extension
        if (!newWalletFilename.contains(".")) {
            // add wallet file extension
            newWalletFilename = newWalletFilename + "." + MultiBitModel.WALLET_FILE_EXTENSION;
        }

        File newWalletFile = new File(newWalletFilename);

        try {
            // if file exists, load the existing wallet
            if (newWalletFile.exists()) {
                PerWalletModelData perWalletModelData = controller.getFileHandler().loadFromFile(newWalletFile);
                if (perWalletModelData != null) {
                    // use the existing wallet
                    controller.addWalletFromFilename(newWalletFile.getAbsolutePath());
                    controller.getModel().setActiveWalletByFilename(newWalletFilename);
                    controller.getModel().setUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");
                    controller.fireRecreateAllViews(true);
                    controller.fireDataChanged();
                }
            } else {
                // create a new wallet
                Wallet newWallet = new Wallet(controller.getMultiBitService().getNetworkParameters());
                ECKey newKey = new ECKey();
                newWallet.keychain.add(newKey);
                PerWalletModelData perWalletModelData = new PerWalletModelData();
                perWalletModelData.setWalletInfo(new WalletInfo(newWalletFilename));
                perWalletModelData.setWallet(newWallet);
                perWalletModelData.setWalletFilename(newWalletFilename);
                perWalletModelData.setWalletDescription(controller.getLocaliser().getString(
                        "createNewWalletSubmitAction.defaultDescription"));
                controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

                // start using the new file as the wallet
                controller.addWalletFromFilename(newWalletFile.getAbsolutePath());
                controller.getModel().setActiveWalletByFilename(newWalletFilename);
                controller.getModel().setUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");

                // save the user properties to disk
                FileHandler.writeUserPreferences(controller);
                log.debug("User preferences with new wallet written successfully");

                controller.fireRecreateAllViews(true);
                controller.fireDataChanged();
            }
        } catch (IOException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated", new Object[]{newWalletFilename, e.getMessage()});
            log.error(message);
            controller.updateStatusLabel(message);  
        } catch (WalletInfoException e) {
            message = controller.getLocaliser().getString("createNewWalletAction.walletCouldNotBeCreated", new Object[]{newWalletFilename, e.getMessage()});
            log.error(message);
            controller.updateStatusLabel(message);  
        }
    }
}