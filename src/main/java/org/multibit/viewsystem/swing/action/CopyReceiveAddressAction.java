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

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.ReceiveBitcoinPanel;

/**
 * This {@link Action} represents the swing copy receive address action
 */
public class CopyReceiveAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private ReceiveBitcoinPanel receiveBitcoinPanel;

    /**
     * Creates a new {@link CopyReceiveAddressAction}.
     * 
     * @param copyIcon
     */
    public CopyReceiveAddressAction(Controller controller, ReceiveBitcoinPanel receiveBitcoinPanel, ImageIcon copyIcon) {
        super("", copyIcon);
        this.receiveBitcoinPanel = receiveBitcoinPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("copyAddressAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyAddressAction.mnemonicKey"));
    }

    /**
     * Copy receive address to clipboard
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        // copy receive address to clipboard
        TextTransfer textTransfer = new TextTransfer();
        textTransfer.setClipboardContents(receiveBitcoinPanel.getReceiveAddress());
    }
}