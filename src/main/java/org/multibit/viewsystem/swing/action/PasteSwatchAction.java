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

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.utils.WhitespaceTrimmer;
import org.multibit.viewsystem.swing.view.panels.AbstractTradePanel;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.io.IOException;

/**
 * This {@link Action} enables you to paste a swatch into the Send Bitcoin Panel
 */
public class PasteSwatchAction extends AbstractAction {

    private static final long serialVersionUID = 193452235465057705L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private AbstractTradePanel tradePanel;

    /**
     * Creates a new {@link PasteSwatchAction}.
     */
    public PasteSwatchAction(BitcoinController bitcoinController, AbstractTradePanel tradePanel, Icon icon) {
        super("", icon);
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.tradePanel = tradePanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("pasteSwatchAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("pasteSwatchAction.mnemonicKey"));
    }

    /**
     * Paste the swatch data
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        // see if an image was pasted
        Image image = getImageFromClipboard();
        if (image != null) {
            if (tradePanel != null) {
                tradePanel.processDroppedImage(image);
            }
        } else {
            // see if text was pasted
            TextTransfer textTransfer = new TextTransfer();
            String stringToPaste = textTransfer.getClipboardContents();
            if (stringToPaste != null) {
                // some text was pasted
                stringToPaste = WhitespaceTrimmer.trim(stringToPaste);

                if (tradePanel != null) {
                    tradePanel.processDecodedString(stringToPaste, null);
                }
            }
        }

        // forward back to the view currently being displayed
        controller.displayView(controller.getCurrentView());
    }

    private Image getImageFromClipboard() {
        Transferable transferable = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
        if (transferable != null && transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
            try {
                return (Image) transferable.getTransferData(DataFlavor.imageFlavor);
            } catch (UnsupportedFlavorException e) {
                // handle this as desired
                e.printStackTrace();
            } catch (IOException e) {
                // handle this as desired
                e.printStackTrace();
            }
        }
        return null;
    }
}