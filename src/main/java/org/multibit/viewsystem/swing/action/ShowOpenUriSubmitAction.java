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
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.ShowOpenUriDialog;

/**
 * This {@link Action} processes the open uri command
 */
public class ShowOpenUriSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;
    private ShowOpenUriDialog showOpenUriDialog;

    /**
     * Creates a new {@link ShowOpenUriSubmitAction}.
     */
    public ShowOpenUriSubmitAction(MultiBitFrame mainFrame, MultiBitController controller, DataProvider dataProvider, ShowOpenUriDialog showOpenUriDialog) {
        super(controller.getLocaliser().getString("showOpenUriView.yesText"));
        this.controller = controller;
        this.dataProvider = dataProvider;
        this.showOpenUriDialog = showOpenUriDialog;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showOpenUriViewAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showOpenUriViewAction.mnemonicKey"));
    }

    /**
     * delegate to generic showOpenUriSubmitAction
     */
    public void actionPerformed(ActionEvent e) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            org.multibit.action.ShowOpenUriSubmitAction submitAction = new org.multibit.action.ShowOpenUriSubmitAction(
                    controller, showOpenUriDialog);
            submitAction.execute(dataProvider);
        }
    }
}