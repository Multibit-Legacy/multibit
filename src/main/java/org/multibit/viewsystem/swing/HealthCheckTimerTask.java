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
package org.multibit.viewsystem.swing;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.store.WalletVersionException;
import org.multibit.viewsystem.swing.action.ExitAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Iterator;
import java.util.List;
import java.util.TimerTask;

/**
 * TimerTask to regularly check the 'health' of MultiBit.
 * It detects whether wallet files have been changed by some external process and to save dirty files.
 * It checks the ping times of the peers to see if they are up.
 *
 * @see java.util.Timer
 * @see java.util.TimerTask
 */
public class HealthCheckTimerTask extends TimerTask {

  public static final int INITIAL_DELAY = 30000; // milliseconds
  public static final int DEFAULT_REPEAT_RATE = 90000; // milliseconds

  private static Logger log = LoggerFactory.getLogger(HealthCheckTimerTask.class);

  private final Controller controller;
  private final BitcoinController bitcoinController;

  private boolean isRunning = false;

  /**
   * Constructs the object, sets the string to be output in function run()
   *
   * @param bitcoinController Controller giving access to Bitcoin object model
   */
  public HealthCheckTimerTask(BitcoinController bitcoinController) {
    this.bitcoinController = bitcoinController;
    this.controller = bitcoinController;
  }

  /**
   * When the timer executes, this code is run.
   */
  @Override
  public void run() {
    isRunning = true;
    try {
      log.debug("Start of HealthCheckTimerTask");

      // See if the ExitAction is running - if so let that persist dirty wallets
      if (ExitAction.isRunning()) {
        log.debug("Skipping HealthCheckTimerTask#run as the ExitAction is already running");
      } else {
        log.debug("Checking if wallets are dirty . . .");
        List<WalletData> perWalletModelDataList = bitcoinController.getModel().getPerWalletModelDataList();

        if (perWalletModelDataList != null) {
          Iterator<WalletData> iterator = perWalletModelDataList.iterator();
          while (iterator.hasNext()) {
            WalletData loopModelData = iterator.next();
            if (bitcoinController.getFileHandler() != null) {
              // See if they are dirty - write out if so.
              if (loopModelData.isDirty()) {
                log.debug("Saving dirty wallet '" + loopModelData.getWalletFilename() + "'...");
                try {
                  bitcoinController.getFileHandler().savePerWalletModelData(loopModelData, false);
                  log.debug("... done.");
                } catch (WalletSaveException | WalletVersionException e) {
                  String message = controller.getLocaliser().getString(
                          "createNewWalletAction.walletCouldNotBeCreated",
                          new Object[]{loopModelData.getWalletFilename(), e.getMessage()});
                  log.error(message);
                  MessageManager.INSTANCE.addMessage(new Message(message));
                }
              }
            }
          }
        }

        log.debug("End of HealthCheckTimerTask");
      }
    } catch (java.util.ConcurrentModificationException cme) {
      log.error("The list of open wallets was changed whilst files were being written.");
    } finally {
      isRunning = false;
    }
  }

  public boolean isRunning() {
    return isRunning;
  }
}