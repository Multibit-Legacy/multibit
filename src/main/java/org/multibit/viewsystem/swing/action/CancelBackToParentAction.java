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
import javax.swing.ImageIcon;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;

/**
 * This {@link Action} represents a cancel action and closes the dialog it is
 * called on
 */
public class CancelBackToParentAction extends AbstractAction {

    private static final long serialVersionUID = 191354565461234705L;

    private MultiBitDialog dialog;

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(Controller controller, ImageIcon icon, MultiBitDialog dialog) {
        super(controller.getLocaliser().getString("cancelBackToParentAction.text"), icon);
        this.dialog = dialog;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("cancelBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("canceBackToParentAction.mnemonicKey"));
    }

    /**
     * close the root dialog
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (dialog != null) {
            dialog.setVisible(false);
        }
    }
}