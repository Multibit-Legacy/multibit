/**
 * Copyright 2013 multibit.org
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

import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * This {@link Action} closes the active wallet.
 */
public class CloseWalletAction extends MultiBitSubmitAction {
    private static final long serialVersionUID = 1923933460523457765L;

    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link CloseWalletAction}.
     */
    public CloseWalletAction(MultiBitController controller, ImageIcon icon, MultiBitFrame mainFrame) {
        super(controller, "closeWalletAction.text", "closeWalletAction.tooltip", "closeWalletAction.mnemonicKey", icon);
        this.controller = controller;
        this.mainFrame = mainFrame;
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("closeWalletAction.tooltip")));
    }

    /**
     * Close the active wallet.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }
        
        try {
            // Close the wallet.
            PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
            
            // Unhook it from the PeerGroup.
            controller.getMultiBitService().getPeerGroup().removeWallet(perWalletModelData.getWallet());
            
            // Save it.
            FileHandler fileHandler = new FileHandler(controller);
            fileHandler.savePerWalletModelData(perWalletModelData, true);
            
            // Remove it from the model.
            controller.getModel().remove(perWalletModelData);
            
            // Set the first wallet to be the active wallet.
            if (!controller.getModel().getPerWalletModelDataList().isEmpty()) {
                PerWalletModelData firstPerWalletModelData = controller.getModel().getPerWalletModelDataList().get(0);
                controller.getModel().setActiveWalletByFilename(firstPerWalletModelData.getWalletFilename());  
            }
        } finally {
            controller.fireRecreateAllViews(true);
        }
    }
}