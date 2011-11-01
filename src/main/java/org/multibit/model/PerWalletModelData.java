package org.multibit.model;

import java.util.List;

import com.google.bitcoin.core.Wallet;

/**
 * this wrapper class wraps all the data pertaining to a single wallet
 * 
 * @author jim
 * 
 */
public class PerWalletModelData {

    private Wallet wallet;
    private String walletFilename;
    private WalletInfo walletInfo;
    private List<WalletTableData> walletTableDataList;

    /**
     * the wallet filename used for when a backup is being written to
     */
    private String walletBackupFilename;

    /**
     * the wallet info filename used for when a backup is being written to
     */

    private String walletInfoBackupFilename;

    /**
     * the files that store the PerWalletModelData have been changed by some
     * other process i.e. NOT this copy of MultiBit
     */
    private boolean filesHaveBeenChangedByAnotherProcess;

    /**
     * the PerWalletModelData (except for incoming receipts) has changed since last been written to disk
     */
    private boolean isDirty;
    
    /**
     * the PerWalletModelData has received incoming receipts since last been written to disk
     */
    private boolean isTransactionDirty;

    public PerWalletModelData() {
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

    public WalletInfo getWalletInfo() {
        return walletInfo;
    }

    public void setWalletInfo(WalletInfo walletInfo) {
        this.walletInfo = walletInfo;
    }

    public List<WalletTableData> getWalletTableDataList() {
        return walletTableDataList;
    }

    public void setWalletTableDataList(List<WalletTableData> walletTableDataList) {
        this.walletTableDataList = walletTableDataList;
    }

    public String getWalletDescription() {
        if (walletInfo != null) {
            return walletInfo.getProperty(WalletInfo.DESCRIPTION_PROPERTY);
        } else {
            return "";
        }
    }

    public void setWalletDescription(String walletDescription) {
        if (walletInfo != null) {
            if (walletDescription == null) {
                walletDescription = "";
            }
            String currentWalletDescription = walletInfo.getProperty(WalletInfo.DESCRIPTION_PROPERTY);
            if (!walletDescription.equals(currentWalletDescription)) {
                walletInfo.put(WalletInfo.DESCRIPTION_PROPERTY, walletDescription);
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

    public boolean isTransactionDirty() {
        return isTransactionDirty;
    }

    public void setTransactionDirty(boolean isTransactionDirty) {
        this.isTransactionDirty = isTransactionDirty;
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
}
