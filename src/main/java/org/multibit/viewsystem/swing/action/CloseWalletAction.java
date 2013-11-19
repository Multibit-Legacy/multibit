/**
 * Copyright 2013 multibit.org
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

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.FileHandler;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.List;

/**
 * This {@link Action} closes the active wallet.
 */
public class CloseWalletAction extends MultiBitSubmitAction {
  private static final Logger log = LoggerFactory.getLogger(CloseWalletAction.class);


  private static final long serialVersionUID = 1923933460523457765L;
  private MultiBitFrame mainFrame;

  private static final int RECREATE_ALL_VIEWS_WAIT_TIME = 500; // ms

  /**
   * Creates a new {@link CloseWalletAction}.
   */
  public CloseWalletAction(BitcoinController bitcoinController, ImageIcon icon, MultiBitFrame mainFrame) {
    super(bitcoinController, "closeWalletAction.text", "closeWalletAction.tooltip", "closeWalletAction.mnemonicKey", icon);
    this.mainFrame = mainFrame;
    putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("closeWalletAction.tooltip")));
  }

  /**
   * Close the active wallet.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (abort()) {
      return;
    }

    boolean successfullyClosedWallet = false;
    String walletToCloseFilename = "";
    try {
      if (mainFrame != null) {
        if (EventQueue.isDispatchThread()) {
          mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

        } else {
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            }
          });
        }
      }

      // Close the wallet.
      WalletData perWalletModelData = this.bitcoinController.getModel().getActivePerWalletModelData();
      walletToCloseFilename = perWalletModelData.getWalletFilename();

      // Unhook it from the PeerGroup.
      super.bitcoinController.getMultiBitService().getPeerGroup().removeWallet(perWalletModelData.getWallet());

      // Save it.
      FileHandler fileHandler = new FileHandler(super.bitcoinController);
      fileHandler.savePerWalletModelData(perWalletModelData, true);

      // Work out which wallet to select after the wallet is removed.
      String activeWalletFilename = perWalletModelData.getWalletFilename();

      List<WalletData> perWalletModelDataList = this.bitcoinController.getModel().getPerWalletModelDataList();
      int numberOfOpenWalletsBefore = perWalletModelDataList.size();
      int positionInList = -1;
      for (int i = 0; i < numberOfOpenWalletsBefore; i++) {
        if (activeWalletFilename.equals(perWalletModelDataList.get(i).getWalletFilename())) {
          positionInList = i;
          break;
        }
      }

      // By default select the first wallet.
      int newWalletToSelect = 0;

      if (numberOfOpenWalletsBefore > 1) {
        // If removing the last, then select the new last one.
        if (positionInList == numberOfOpenWalletsBefore - 1) {
          newWalletToSelect = numberOfOpenWalletsBefore - 2;
        } else {
          // Select the same position in the list
          newWalletToSelect = positionInList;
        }
      } else {
        // One wallet open before. None after.
      }

      // Remove it from the model.
      this.bitcoinController.getModel().remove(perWalletModelData);

      // Set the new Wallet to be the active wallet.
      if (!this.bitcoinController.getModel().getPerWalletModelDataList().isEmpty()) {
        WalletData firstPerWalletModelData = this.bitcoinController.getModel().getPerWalletModelDataList().get(newWalletToSelect);
        this.bitcoinController.getModel().setActiveWalletByFilename(firstPerWalletModelData.getWalletFilename());
      } else {
        // No wallets are selected.
        // Clear all the views.
      }

      successfullyClosedWallet = true;

    } finally {
      controller.fireRecreateAllViews(true);

      // Wait a while for the UI to reinitialise and then set the status message
      // Give the user a message that wallet is closed.
      final String finalWalletToCloseFilename = walletToCloseFilename;
      final boolean finalSuccessfullyClosedWallet = successfullyClosedWallet;
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          if (finalSuccessfullyClosedWallet) {
            try {
              Thread.sleep(RECREATE_ALL_VIEWS_WAIT_TIME);
            } catch (InterruptedException e1) {
              log.error("Unexpected thread interruption", e1);
            }
            Message closeMessage = new Message(controller.getLocaliser().getString("multiBit.closedWallet", new Object[]{finalWalletToCloseFilename}));
            MessageManager.INSTANCE.addMessage(closeMessage);
          }

          // Reset wait cursor
          if (mainFrame != null) {
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
          }
        }
      });
    }
  }
}