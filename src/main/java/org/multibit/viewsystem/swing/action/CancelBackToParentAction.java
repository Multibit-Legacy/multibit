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

import org.multibit.controller.MultiBitController;

/**
 * This {@link Action} represents a cancel action to go back to the parent view
 */
public class CancelBackToParentAction extends AbstractAction {

    private static final long serialVersionUID = 191354565461234705L;

    private MultiBitController controller;

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(MultiBitController controller) {
        this(controller, null);
    }

    /**
     * Creates a new {@link CancelBackToParentAction}.
     */
    public CancelBackToParentAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("cancelBackToParentAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("cancelBackToParentAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("canceBackToParentAction.mnemonicKey"));
    }

    /**
     * delegate to the generic CancelBackToParentAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.CancelBackToParentAction cancelBackToParentAction = 
            new org.multibit.action.CancelBackToParentAction(controller);
        cancelBackToParentAction.execute(null);
    }
}