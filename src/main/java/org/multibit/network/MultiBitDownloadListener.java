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

package org.multibit.network;

import com.google.bitcoin.core.DownloadListener;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanelDownloadListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


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
public class MultiBitDownloadListener extends DownloadListener {
    private static final Logger log = LoggerFactory.getLogger(MultiBitDownloadListener.class);

    public static final double DONE_FOR_DOUBLES = 99.99; // not quite 100 per
                                                          // cent to cater for
                                                          // rounding
    private static final int CRITERIA_LARGE_NUMBER_OF_BLOCKS = 1000;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private List<SingleWalletPanelDownloadListener> singleWalletPanelDownloadListeners;

    private Object lockObject = new Object();
    
    private int blocksToDownloadAtStart = -1;

    public MultiBitDownloadListener(BitcoinController controller) {
        this.bitcoinController = controller;
        this.controller = this.bitcoinController;
        this.singleWalletPanelDownloadListeners = new ArrayList<SingleWalletPanelDownloadListener>();
    }

    public void addSingleWalletPanelDownloadListener(SingleWalletPanelDownloadListener downloadListener) {
        singleWalletPanelDownloadListeners.add(downloadListener);
    }
    
    public void removeDownloadListener(SingleWalletPanelDownloadListener downloadListener) {
        singleWalletPanelDownloadListeners.remove(downloadListener);
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
    protected void progress(double pct, int blocksSoFar, Date date) {
        if (pct > DONE_FOR_DOUBLES) {
            // we are done downloading
            doneDownload();
        } else {
            //log.debug("Download - blocksSoFar = " + blocksSoFar);
            synchronized (lockObject) {
                String downloadStatusText = controller.getLocaliser().getString("multiBitDownloadListener.startDownloadTextShort",
                        new Object[] {blocksSoFar})
                        + " "
                        + controller.getLocaliser().getString(
                                "multiBitDownloadListener.blockDateText",
                                new Object[] { DateFormat.getDateInstance(DateFormat.MEDIUM, controller.getLocaliser().getLocale())
                                        .format(date) });

                // Work out the percent as the total amount of blocks at beginning of the replay task.
                // (If a peer dies and a new one starts the download listener percents are based on the
                // total that peer knows about which is confusing in the UI).
                ReplayTask currentReplayTask = ReplayManager.INSTANCE.getCurrentReplayTask();
                if (currentReplayTask != null) {
                    if (currentReplayTask.getStartHeight() != ReplayTask.UNKNOWN_START_HEIGHT) {
                        double denominator = Math.max(blocksToDownloadAtStart, ReplayManager.INSTANCE.getActualLastChainHeight() - currentReplayTask.getStartHeight());
                        pct = (int)(100 * ( 1.0 - (double)blocksSoFar/denominator));
                    }
                    //log.debug("blocksSoFar = " + blocksSoFar + ", actualLastChainHeight = " + ReplayManager.INSTANCE.getActualLastChainHeight() +
                    //        ", startHeight = " + currentReplayTask.getStartHeight() + ", percent = " + pct);
                }
                
                // When busy occasionally the localiser fails to localise.
                if (!(downloadStatusText.indexOf("multiBitDownloadListener") > -1)) {
                    Message message = new Message(downloadStatusText, pct);
                    MessageManager.INSTANCE.addMessage(message);
                }
                
                for (SingleWalletPanelDownloadListener singleWalletPanelDownloadListener : singleWalletPanelDownloadListeners) {
                    singleWalletPanelDownloadListener.progress(pct, blocksSoFar, date);
                }
            }
            this.bitcoinController.fireBlockDownloaded();
        }
    }

    /**
     * Called when download is initiated.
     * 
     * @param blocks
     *            the number of blocks to download, estimated
     */
    @Override
    protected void startDownload(int blocks) {
        if (blocks == 0) {
            doneDownload();
        } else {
            synchronized (lockObject) {
                blocksToDownloadAtStart = blocks;
                
                String startDownloadText;
                if (blocks <= CRITERIA_LARGE_NUMBER_OF_BLOCKS) {
                    startDownloadText = controller.getLocaliser().getString("multiBitDownloadListener.startDownloadTextShort",
                            new Object[] {blocks});
                } else {
                    startDownloadText = controller.getLocaliser().getString("multiBitDownloadListener.startDownloadTextLong",
                            new Object[] {blocks});
                }

                String startDownloadTextForLabel = controller.getLocaliser().getString("multiBitDownloadListener.downloadingText");

                // When busy occasionally the localiser fails to localise
                if (!(startDownloadText.indexOf("multiBitDownloadListener") > -1)) {
                    Message message = new Message(startDownloadTextForLabel, Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
                    MessageManager.INSTANCE.addMessage(message);
                    message = new Message(startDownloadTextForLabel, 0);
                    MessageManager.INSTANCE.addMessage(message);
                }
                
                for (SingleWalletPanelDownloadListener singleWalletPanelDownloadListener : singleWalletPanelDownloadListeners) {
                    singleWalletPanelDownloadListener.startDownload(blocks);
                }
            }
            this.bitcoinController.fireBlockDownloaded();
        }
    }

    /**
     * Called when we are done downloading the block chain.
     */
    @Override
    protected void doneDownload() {
        ReplayManager.INSTANCE.taskHasCompleted(null);

        String downloadStatusText = controller.getLocaliser().getString("multiBitDownloadListener.doneDownloadText");
     
        Message message = new Message(downloadStatusText, true, Message.NOT_RELEVANT_PERCENTAGE_COMPLETE);
        MessageManager.INSTANCE.addMessage(message);

        message = new Message(downloadStatusText, 100);
        MessageManager.INSTANCE.addMessage(message);

        for (SingleWalletPanelDownloadListener singleWalletPanelDownloadListener : singleWalletPanelDownloadListeners) {
            singleWalletPanelDownloadListener.doneDownload();
        }
                
        this.bitcoinController.fireBlockDownloaded();
    }
}