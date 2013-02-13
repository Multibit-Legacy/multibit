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
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;

/**
 * This {@link Action} represents the swing copy address action
 */
public class CopySendAddressAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private SendBitcoinPanel sendBitcoinPanel;

    /**
     * Creates a new {@link CopySendAddressAction}.
     */
    public CopySendAddressAction(Controller controller, SendBitcoinPanel sendBitcoinPanel, ImageIcon icon) {
        super("", icon);
        this.sendBitcoinPanel = sendBitcoinPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("copyAddressAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyAddressAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy send address text action
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        // copy to clipboard
        TextTransfer textTransfer = new TextTransfer();
        textTransfer.setClipboardContents(sendBitcoinPanel.getAddress());
    }
}