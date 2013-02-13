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

import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.TransferHandler;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.dataproviders.CopyQRCodeImageDataProvider;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * This {@link Action} represents the swing copy QRCode image action
 */
public class CopyQRCodeImageAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private CopyQRCodeImageDataProvider dataProvider;

    /**
     * Creates a new {@link CopyQRCodeImageAction}.
     */
    public CopyQRCodeImageAction(Controller controller, CopyQRCodeImageDataProvider dataProvider, Icon icon) {
        super("", icon);
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("copyQRCodeImageAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("copyQRCodeImageAction.mnemonicKey"));
    }

    /**
     * delegate to generic copy QRCode image action
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (dataProvider != null) {
            JLabel qrCodeLabel = dataProvider.getURIImage();

            final Clipboard clipboard = qrCodeLabel.getTopLevelAncestor().getToolkit().getSystemClipboard();

            // copy to clipboard
            TransferHandler handler = qrCodeLabel.getTransferHandler();
            handler.exportToClipboard(qrCodeLabel, clipboard, TransferHandler.COPY);
        }
    }
}