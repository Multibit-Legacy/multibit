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
package org.multibit.model.bitcoin;

import java.util.List;
import java.util.UUID;

import org.multibit.viewsystem.swing.view.walletlist.SingleWalletPanelDownloadListener;

import com.google.bitcoin.core.Wallet;

/**
 * This wrapper class wraps all the data pertaining to a single wallet.
 * 
 * @author jim
 * 
 */
public class WalletData {

    private Wallet wallet;
    private String walletFilename;
    private WalletInfoData walletInfo;
    private List<WalletTableData> walletTableDataList;
    
    /**
     * The wallet filename used for when a backup is being written to.
     */
    private String walletBackupFilename;

    /**
     * The wallet info filename used for when a backup is being written to.
     */

    private String walletInfoBackupFilename;

    /**
     * The files that store the WalletData have been changed by some
     * other process i.e. NOT this copy of MultiBit.
     */
    private transient boolean filesHaveBeenChangedByAnotherProcess;

    /**
     * The WalletData has changed since last been written to disk.
     */
    private transient boolean isDirty;
    
    /**
     * This wallet is currently busy with an operation that affects the private keys.
     * (Note this is a UI hint rather than a hard lock. Synchronize on wallet.
     */
    private transient boolean busy;
    
    /**
     * The localisation key for the name of the operation that is currently operating on the wallets private keys.
     * This is typically a sentence in length.
     */
    private transient String busyTaskKey;
    
    /**
     * The present continuous verb describing the current busy operation. The localisation key.
     * This is something like the key for "Synchronising...", "Waiting...", "Importing...".
     * It is displayed instead of the balance amount so needs to be short.
     */
    private transient String busyTaskVerbKey;
    
    /**
     * The UUID of the replay task that is being running on this wallet.
     */
    private transient UUID replayTaskUUID;
    
    /**
     * A listener used by the SingleWalletPanel to receive updates on busy status.
     */
    private transient SingleWalletPanelDownloadListener singleWalletDownloadListener;

    
    public WalletData() {
        isDirty = false;
    }

    public Wallet getWallet() {
        return wallet;
    }

    public void setWallet(Wallet wallet) {
        this.wallet = wallet;
    }

    public String getWalletFilename() {
        return walletFilename;
    }

    public void setWalletFilename(String walletFilename) {
        this.walletFilename = walletFilename;
    }

    public WalletInfoData getWalletInfo() {
        return walletInfo;
    }

    public void setWalletInfo(WalletInfoData walletInfo) {
        this.walletInfo = walletInfo;
        if (walletInfo != null && wallet != null) {
            if (wallet.getVersion() == null) {
                wallet.setVersion(walletInfo.getWalletVersion());
            }
        }
    }

    public List<WalletTableData> getWalletTableDataList() {
        return walletTableDataList;
    }

    public void setWalletTableDataList(List<WalletTableData> walletTableDataList) {
        this.walletTableDataList = walletTableDataList;
    }

    public String getWalletDescription() {
        if (walletInfo != null) {
            return walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
        } else {
            return "";
        }
    }

    public void setWalletDescription(String walletDescription) {
        if (walletInfo != null) {
            if (walletDescription == null) {
                walletDescription = "";
            }
            String currentWalletDescription = walletInfo.getProperty(WalletInfoData.DESCRIPTION_PROPERTY);
            if (!walletDescription.equals(currentWalletDescription)) {
                walletInfo.put(WalletInfoData.DESCRIPTION_PROPERTY, walletDescription);
                if (wallet != null) {
                    wallet.setDescription(walletDescription);
                }
                
                setDirty(true);
            }
        }
    }

    public boolean isFilesHaveBeenChangedByAnotherProcess() {
        return filesHaveBeenChangedByAnotherProcess;
    }

    public void setFilesHaveBeenChangedByAnotherProcess(boolean filesHaveBeenChangedByAnotherProcess) {
        this.filesHaveBeenChangedByAnotherProcess = filesHaveBeenChangedByAnotherProcess;
    }

    public boolean isDirty() {
        return isDirty;
    }

    public void setDirty(boolean isDirty) {
        this.isDirty = isDirty;
    }

    public String getWalletBackupFilename() {
        return walletBackupFilename;
    }

    public void setWalletBackupFilename(String walletBackupFilename) {
        this.walletBackupFilename = walletBackupFilename;
    }

    public String getWalletInfoBackupFilename() {
        return walletInfoBackupFilename;
    }

    public void setWalletInfoBackupFilename(String walletInfoBackupFilename) {
        this.walletInfoBackupFilename = walletInfoBackupFilename;
    }

    public boolean isBusy() {
        return busy;
    }

    public void setBusy(boolean busy) {
        this.busy = busy;
    }

    public String getBusyTaskKey() {
        return busyTaskKey;
    }

    public void setBusyTaskKey(String busyTaskKey) {
        this.busyTaskKey = busyTaskKey;
    }

    public String getBusyTaskVerbKey() {
        return busyTaskVerbKey;
    }

    public void setBusyTaskVerbKey(String busyTaskVerbKey) {
        this.busyTaskVerbKey = busyTaskVerbKey;
    }

    public void setSingleWalletDownloadListener(SingleWalletPanelDownloadListener singleWalletDownloadListener) {
        this.singleWalletDownloadListener = singleWalletDownloadListener;
    }

    public SingleWalletPanelDownloadListener getSingleWalletDownloadListener() {
        return singleWalletDownloadListener;
    }

    public UUID getReplayTaskUUID() {
        return replayTaskUUID;
    }

    public void setReplayTaskUUID(UUID replayTaskUUID) {
        this.replayTaskUUID = replayTaskUUID;
    }
}
