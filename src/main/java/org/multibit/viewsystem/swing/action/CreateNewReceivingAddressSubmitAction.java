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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JPasswordField;

import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.model.WalletVersion;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressDialog;
import org.multibit.viewsystem.swing.view.CreateNewReceivingAddressPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.WalletType;

/**
 * This {@link Action} represents an action to actually create receiving addresses.
 */
public class CreateNewReceivingAddressSubmitAction extends AbstractAction {
    private static Logger log = LoggerFactory.getLogger(CreateNewReceivingAddressAction.class);
    
    private static final long serialVersionUID = 200152235465875405L;

    private MultiBitController controller;

    private CreateNewReceivingAddressDialog createNewReceivingAddressDialog;
    private CreateNewReceivingAddressPanel createNewReceivingAddressPanel;
    
    private JPasswordField walletPassword;

    /**
     * Creates a new {@link CreateNewReceivingAddressSubmitAction}.
     */
    public CreateNewReceivingAddressSubmitAction(MultiBitController controller, CreateNewReceivingAddressDialog createNewReceivingAddressDialog, CreateNewReceivingAddressPanel createNewReceivingAddressPanel, JPasswordField walletPassword) {
        super(controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.text"), ImageLoader.createImageIcon(ImageLoader.ADD_ICON_FILE));
        this.controller = controller;
        this.createNewReceivingAddressDialog = createNewReceivingAddressDialog;
        this.createNewReceivingAddressPanel = createNewReceivingAddressPanel;
        this.walletPassword = walletPassword;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createNewReceivingAddressSubmitAction.mnemonicKey"));
    }

    /**
     * Create new receiving address.
     */
    public void actionPerformed(ActionEvent e) {
        // Check to see if the wallet files have changed.
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        boolean encryptNewKeys = false;
        
        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            if (controller.getModel().getActiveWallet() != null) {
                if (controller.getModel().getActiveWallet().getWalletType() == WalletType.ENCRYPTED) {
                    if (walletPassword.getPassword() == null || walletPassword.getPassword().length == 0) {
                        // User needs to enter password.
                        createNewReceivingAddressPanel.setMessageText(controller.getLocaliser().getString("showExportPrivateKeysAction.youMustEnterTheWalletPassword"));
                        return;
                    }
                    encryptNewKeys = true;
                                    
                    if (!controller.getModel().getActiveWallet().checkPasswordCanDecryptFirstPrivateKey(walletPassword.getPassword())) {
                        // The password supplied is incorrect.
                        createNewReceivingAddressPanel.setMessageText(controller.getLocaliser().getString("createNewReceivingAddressSubmitAction.passwordIsIncorrect"));
                        return;
                    }
                } 
            }
                       
            WalletInfo walletInfo = perWalletModelData.getWalletInfo();
            if (walletInfo == null) {
                walletInfo = new WalletInfo(perWalletModelData.getWalletFilename(), WalletVersion.PROTOBUF);
                perWalletModelData.setWalletInfo(walletInfo);
            }
            String addressString = "";
            try {
                for (int i = 0; i < createNewReceivingAddressPanel.getNumberOfAddressesToCreate(); i++) {
                    ECKey newKey;
                    if (encryptNewKeys) {
                        // Use the wallet EncrypterDescrypter.
                        newKey = new ECKey(perWalletModelData.getWallet().getEncrypterDecrypter());
                        newKey.encrypt(walletPassword.getPassword());
                    } else {
                        newKey = new ECKey();
                    }
                    perWalletModelData.getWallet().keychain.add(newKey);
                    addressString = newKey.toAddress(controller.getModel().getNetworkParameters()).toString();
                    walletInfo.addReceivingAddress(new AddressBookData("", addressString), false);
                }
            } catch (EncrypterDecrypterException ede) {
                log.error(ede.getMessage(), ede);
                createNewReceivingAddressPanel.setMessageText(ede.getMessage());
            }
            
            if (createNewReceivingAddressPanel.getReceiveBitcoinPanel() != null) {
                createNewReceivingAddressPanel.getReceiveBitcoinPanel().getAddressesTableModel().fireTableDataChanged();
                createNewReceivingAddressPanel.getReceiveBitcoinPanel().selectRows();
            }
            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, addressString);
            controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, "");
            
            try {
                controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
            } catch (WalletSaveException wse) {
                log.error(wse.getClass().getCanonicalName() + " " + wse.getMessage());
                MessageManager.INSTANCE.addMessage(new Message(wse.getClass().getCanonicalName() + " " + wse.getMessage()));
            }
            controller.displayView(controller.getCurrentView());
            if (createNewReceivingAddressDialog != null) {
                createNewReceivingAddressDialog.setVisible(false);
            }

            if (createNewReceivingAddressPanel.getReceiveBitcoinPanel() != null && createNewReceivingAddressPanel.getReceiveBitcoinPanel().getLabelTextArea() != null) {
                createNewReceivingAddressPanel.getReceiveBitcoinPanel().getLabelTextArea().requestFocusInWindow();
            }
        }
    }
}