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
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} exits the application.
 */
public class ExitAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460565057705L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    
    /**
     * Creates a new {@link ExitAction}.
     */
    public ExitAction(MultiBitController controller, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("exitAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("exitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("exitAction.mnemonicKey"));
    }

    /**
     * delegate to the generic ExitAction
     */
    public void actionPerformed(ActionEvent e) {
        org.multibit.action.ExitAction exitAction = new org.multibit.action.ExitAction(controller, mainFrame);
        exitAction.execute(null);
    }
}