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

import java.awt.Cursor;
import java.awt.event.ActionEvent;

import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.Controller;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.dialogs.DeleteWalletConfirmDialog;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * This {@link Action} show the delete wallet confirmation dialog.
 */
public class DeleteWalletAction extends MultiBitSubmitAction {
    private static final long serialVersionUID = 1923933460523457765L;

    private MultiBitFrame mainFrame;

    /**
     * Creates a new {@link DeleteWalletAction}.
     */
    public DeleteWalletAction(MultiBitController multiBitController, ImageIcon icon, MultiBitFrame mainFrame) {
        super(multiBitController, "deleteWalletAction.text", "deleteWalletAction.tooltip", "deleteWalletAction.mnemonicKey", icon);
        this.mainFrame = mainFrame;
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("deleteWalletAction.tooltip")));
    }

    /**
     * prompt for deletion of a wallet
     */
    public void actionPerformed(ActionEvent e) {
        if (abort()) {
            return;
        }
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);
  
        try {
            DeleteWalletConfirmDialog deleteWalletConfirmDialog = new DeleteWalletConfirmDialog(super.multiBitController, mainFrame);
            deleteWalletConfirmDialog.setVisible(true);
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }
}