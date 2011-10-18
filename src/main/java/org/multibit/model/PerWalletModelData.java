package org.multibit.model;

import java.util.List;

import com.google.bitcoin.core.Wallet;

/**
 * this wrapper class wraps all the data pertaining to a single wallet
 * @author jim
 *
 */
public class PerWalletModelData {

    private Wallet wallet;
    private String walletFilename;
    private WalletInfo walletInfo;
    private  List<WalletTableData> walletTableDataList;
    
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
}
