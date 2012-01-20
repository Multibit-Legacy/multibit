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

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopyQRCodeTextAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;
    
    private DataProvider dataProvider;

    /**
     * Creates a new {@link CopyQRCodeTextAction}.
     */
    public CopyQRCodeTextAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("copyQRCodeTextAction.text"));
        this.dataProvider = dataProvider;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("copyQRCodeTextAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyQRCodeTextAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode text action
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CopyQRCodeTextAction genericQRCodeTextAction = new org.multibit.action.CopyQRCodeTextAction();
        genericQRCodeTextAction.execute(dataProvider);
    }
}