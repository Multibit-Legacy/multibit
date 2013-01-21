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

import org.multibit.controller.MultiBitController;
import org.multibit.model.WalletBusyListener;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;

/**
 * This {@link Action} represents a localised MultiBit action that just displays a view
 * and is a WalletChangeListener
 */
public class MultiBitWalletBusyAction extends AbstractAction implements WalletBusyListener {

    private static final long serialVersionUID = 191948235465057705L;

    private MultiBitController controller;
    private View viewToDisplay;
    private String tooltipKey;

    /**
     * Creates a new {@link MultiBitAction}.
     * @param controller The MultiBitController
     * @param imagePath The relative path to the image to load for the item. Usually an imageLoader constant
     * @param textKey The localisation key for the text of the action
     * @param tooltipKey The localisation key for the tooltip of the action
     * @param mnemonicKey The localisation key for the mnemonic of the action
     * @param viewToDisplay The view to display on action activation.  One of the View constants
     */
    public MultiBitWalletBusyAction(MultiBitController controller, String imagePath, String textKey, String tooltipKey, String mnemonicKey, View viewToDisplay) {
        super(textKey == null ? "" : controller.getLocaliser().getString(textKey), ImageLoader.createImageIcon(imagePath));
        this.controller = controller;
        this.viewToDisplay = viewToDisplay;
        this.tooltipKey = tooltipKey;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString(tooltipKey));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic(mnemonicKey));
        
        controller.registerWalletBusyListener(this);
    }

    /**
     * Display the view specified.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        controller.displayView(viewToDisplay);
    }

    @Override
    public void walletBusyChange(boolean newWalletIsBusy) {
        // Update the tooltip to match the wallet busy status.
        // Enable/ disable is done by MultiBitFrame.
        if (newWalletIsBusy) {
            // Wallet is busy with another operation that may change the private keys - Action is disabled.
            putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("multiBitSubmitAction.walletIsBusy", new Object[]{controller.getModel().getActivePerWalletModelData().getBusyOperation()}));          
        } else {
            // Enable unless wallet has been modified by another process.
            if (!controller.getModel().getActivePerWalletModelData().isFilesHaveBeenChangedByAnotherProcess()) {
                putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString(tooltipKey));
            }
        }
    }
}