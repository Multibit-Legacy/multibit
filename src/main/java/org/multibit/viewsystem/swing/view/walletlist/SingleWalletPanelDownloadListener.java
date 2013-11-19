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

package org.multibit.viewsystem.swing.view.walletlist;

import com.google.bitcoin.core.DownloadListener;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.network.ReplayManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;
import java.util.UUID;

/**
 * Listen to chain download events and print useful informational messages.
 * 
 * <p>
 * progress, startDownload, doneDownload maybe be overridden to change the way
 * the user is notified.
 * 
 * <p>
 * Methods are called with the event listener object locked so your
 * implementation does not have to be thread safe.
 * 
 */
public class SingleWalletPanelDownloadListener extends DownloadListener {
    private static final Logger log = LoggerFactory.getLogger(SingleWalletPanelDownloadListener.class);

    private static final double DONE_FOR_DOUBLES = 99.99; // not quite 100 per
                                                          // cent to cater for
                                                          // rounding

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    final SingleWalletPanel singleWalletPanel;
    
    private Object lockObject = new Object();

    public SingleWalletPanelDownloadListener(BitcoinController bitcoinController, SingleWalletPanel singleWalletPanel) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        this.singleWalletPanel = singleWalletPanel;
    }

    /**
     * Called when download progress is made.
     * 
     * @param pct
     *            the percentage of chain downloaded, estimated
     * @param blocksSoFar
     *            number of blocks so far
     * @param date
     *            the date of the last block downloaded
     */
    @Override
    public void progress(double pct, int blocksSoFar, Date date) {
        if (pct > DONE_FOR_DOUBLES) {
            // we are done downloading
            doneDownload();
        } else {
            synchronized (lockObject) {
                String downloadStatusText = controller.getLocaliser().getString("multiBitDownloadListener.downloadingTextShort");

                // When busy occasionally the localiser fails to localise
                if (!(downloadStatusText.indexOf("multiBitDownloadListener") > -1)) {
                    singleWalletPanel.setSyncMessage(downloadStatusText, pct);
                }
            }
        }
    }

    /**
     * Called when download is initiated.
     * 
     * @param blocks
     *            the number of blocks to download, estimated
     */
    @Override
    public void startDownload(int blocks) {
        if (blocks == 0) {
            doneDownload();
        } else {
            synchronized (lockObject) {
                String startDownloadText = controller.getLocaliser().getString("multiBitDownloadListener.downloadingTextShort");
 
                // When busy occasionally the localiser fails to localise.
                if (!(startDownloadText.indexOf("multiBitDownloadListener") > -1)) {
                    singleWalletPanel.setSyncMessage(startDownloadText, Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
                }
            }
        }
    }

    /**
     * Called when we are done downloading the block chain.
     */
    @Override
    public void doneDownload() {
        String downloadStatusText = controller.getLocaliser().getString("multiBitDownloadListener.doneDownloadText");
        singleWalletPanel.setSyncMessage(downloadStatusText, 100);

        // Tell the replay manager this task is finished.
        WalletData perWalletModelData = singleWalletPanel.getPerWalletModelData();
        if (perWalletModelData != null) {
            UUID replayTaskUUID = perWalletModelData.getReplayTaskUUID();
            if (replayTaskUUID != null) {
                ReplayManager.INSTANCE.taskHasCompleted(replayTaskUUID);                
            }
        }
    }
}