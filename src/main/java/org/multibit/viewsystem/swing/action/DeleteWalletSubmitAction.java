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
package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.file.DeleteWalletException;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.model.MultiBitModel;
import org.multibit.model.bitcoin.wallet.WalletData;
import org.multibit.store.MultiBitWalletVersion;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.view.dialogs.DeleteWalletConfirmDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} deletes a wallet.
 */
public class DeleteWalletSubmitAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(DeleteWalletSubmitAction.class);

    private static final long serialVersionUID = 1923933460523457765L;

    private MultiBitController controller;
    private DeleteWalletConfirmDialog deleteWalletConfirmDialog;

    /**
     * Creates a new {@link DeleteWalletSubmitAction}.
     */
    public DeleteWalletSubmitAction(MultiBitController controller, ImageIcon icon, DeleteWalletConfirmDialog deleteWalletConfirmDialog) {
        super(controller.getLocaliser().getString("deleteWalletAction.text"), icon);
        this.controller = controller;
        this.deleteWalletConfirmDialog = deleteWalletConfirmDialog;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("deleteWalletAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("deleteWalletAction.mnemonicKey"));
    }

    /**
     * Delete the wallet and updates the dialog.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            String walletDescription = controller.getModel().getActivePerWalletModelData().getWalletDescription();

            // Delete the wallet.
            boolean newWalletCreated = deleteActiveWallet();
            
            // Set the first wallet to be the active wallet.
            WalletData firstPerWalletModelData = controller.getModel().getPerWalletModelDataList().get(0);
            controller.getModel().setActiveWalletByFilename(firstPerWalletModelData.getWalletFilename());
             
            String confirm2 = newWalletCreated ? controller.getLocaliser().getString("deleteWalletConfirmDialog.newWalletCreated") : " ";
            if (deleteWalletConfirmDialog != null) {
                deleteWalletConfirmDialog.getExplainLabel().setText(" ");
                deleteWalletConfirmDialog.setDeleteConfirmText(
                    controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeletedOk",
                            new Object[] { walletDescription }), confirm2 );
            }            
         } catch (WalletLoadException wle) {
             log.error(wle.getClass().getName() + " " + wle.getMessage());
             if (wle.getCause() != null) {
                 log.error(wle.getClass().getName() + ", cause = " + wle.getCause().getMessage());
             }
             deleteWalletConfirmDialog.getExplainLabel().setText(" ");
             deleteWalletConfirmDialog.setDeleteConfirmText(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError1"), controller
                     .getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[] { wle.getMessage() }));
         } catch (WalletVersionException wve) {
             log.error(wve.getClass().getName() + " " + wve.getMessage());
             if (wve.getCause() != null) {
                 log.error(wve.getClass().getName() + ", cause = " + wve.getCause().getMessage());
             }
             deleteWalletConfirmDialog.getExplainLabel().setText(" ");
             deleteWalletConfirmDialog.setDeleteConfirmText(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError1"), controller
                     .getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[] { wve.getMessage() }));
         } catch (DeleteWalletException dwe) {
            log.error(dwe.getClass().getName() + " " + dwe.getMessage());
            if (dwe.getCause() != null) {
                log.error(dwe.getClass().getName() + ", cause = " + dwe.getCause().getMessage());
            }
            deleteWalletConfirmDialog.getExplainLabel().setText(" ");
            deleteWalletConfirmDialog.setDeleteConfirmText(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError1"), controller
                    .getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[] { dwe.getMessage() }));
        } catch (IOException ioe) {
            log.error(ioe.getClass().getName() + " " + ioe.getMessage());
            if (ioe.getCause() != null) {
                log.error(ioe.getClass().getName() + ", cause = " + ioe.getCause().getMessage());
            }
            deleteWalletConfirmDialog.getExplainLabel().setText(" ");
            deleteWalletConfirmDialog.setDeleteConfirmText(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError1"), controller
                    .getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[] { ioe.getMessage() }));
        } finally {
            controller.fireRecreateAllViews(true);
            controller.fireDataChangedUpdateNow();
        }
    }
    
    /**
     * Delete wallet by filename.
     * 
     * @param filename of wallet to delete
     * @return newWalletCreated
     * @throws DeleteWalletException
     * @throws IOException     */
    public boolean deleteWallet(String filename) throws DeleteWalletException, IOException {
        return deleteWallet(controller.getModel().getPerWalletModelDataByWalletFilename(filename));
    }
    
    /**
     * Delete the active wallet.
     * @return newWalletCreated
     * @throws DeleteWalletException
     * @throws IOException
     */
    public boolean deleteActiveWallet()  throws DeleteWalletException, WalletVersionException, IOException {
        boolean newWalletCreated = false;
        WalletData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        
        MultiBitWalletVersion walletVersion = perWalletModelData.getWalletInfo().getWalletVersion();
        String backupFilename = perWalletModelData.getWalletInfo().getProperty(MultiBitModel.WALLET_BACKUP_FILE);

        newWalletCreated = deleteWallet(controller.getModel().getActivePerWalletModelData());
    
        if (backupFilename != null && !"".equals(backupFilename)) {
            if (MultiBitWalletVersion.PROTOBUF == walletVersion || MultiBitWalletVersion.PROTOBUF_ENCRYPTED == walletVersion) {

                // Delete the backupFile unless the user has manually opened it.
                boolean userHasOpenedBackupFile = false;
                List<WalletData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
                if (perWalletModelDataList != null) {
                    for (WalletData perWalletModelDataLoop : perWalletModelDataList) {
                        if ((backupFilename != null && backupFilename.equals(perWalletModelDataLoop.getWalletFilename()))) {
                            userHasOpenedBackupFile = true;
                            break;
                        }
                    }
                }
                if (!userHasOpenedBackupFile) {
                    FileHandler.secureDelete(new File(backupFilename));
                }
            }
        }
        
        return newWalletCreated;
    }
    
    /**
     * Actually delete the wallet (no UI elements used).
     * @param perWalletModelData of wallet to delete
     * @return newWalletCreated
     * @throws DeleteWalletException
     * @throws IOException
     */
    private boolean deleteWallet(WalletData perWalletModelData) throws DeleteWalletException, WalletVersionException, IOException {
        FileHandler fileHandler = new FileHandler(controller);
        fileHandler.deleteWalletAndWalletInfo(perWalletModelData);

        // If no wallets, create an empty one.
        boolean newWalletCreated = false;
        if (controller.getModel().getPerWalletModelDataList().size() == 0) {
            if (controller.getMultiBitService() != null) {
                controller.getMultiBitService().addWalletFromFilename(null);
              
                newWalletCreated = true;
             }
        }
        
        // Set the first wallet to be the active wallet.
        WalletData firstPerWalletModelData = controller.getModel().getPerWalletModelDataList().get(0);
        controller.getModel().setActiveWalletByFilename(firstPerWalletModelData.getWalletFilename());
        
        fileHandler.savePerWalletModelData(firstPerWalletModelData, true);
        
        // Save the user properties to disk.
        FileHandler.writeUserPreferences(controller);
        log.debug("User preferences with old wallet deleted were written successfully");

        controller.fireRecreateAllViews(true);
        controller.fireDataChangedUpdateNow();
        
        return newWalletCreated;
    }
}