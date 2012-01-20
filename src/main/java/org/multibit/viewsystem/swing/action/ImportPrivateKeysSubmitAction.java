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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;

/**
 * This {@link Action} imports the private keys to the active wallet
 */
public class ImportPrivateKeysSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492087598757765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link ImportPrivateKeysSubmitAction}.
     */
    public ImportPrivateKeysSubmitAction(MultiBitController controller, DataProvider dataProvider, ImageIcon icon) {
        super(controller.getLocaliser().getString("showImportPrivateKeysAction.text"), icon);
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showImportPrivateKeysAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showImportPrivateKeysAction.mnemonicKey"));
    }

    /**
     * delegate to generic submit action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ImportPrivateKeysSubmitAction submitAction = new org.multibit.action.ImportPrivateKeysSubmitAction(controller);
        submitAction.execute(dataProvider);
    }
}