package org.multibit.viewsystem.simple;

import java.math.BigInteger;

import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.ViewSystem;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

/**
 * A very simple implementation of a view system
 */
public class SimpleViewSystem implements ViewSystem {
    int numberOfBlocksDownloaded = 0;

    public int getNumberOfBlocksDownloaded() {
        return numberOfBlocksDownloaded;
    }

    boolean online = false;

    public boolean isOnline() {
        return online;
    }

    @Override
    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger arg2, BigInteger arg3) {
        System.out.println("SIMPLE. Wallet " + wallet.hashCode() + " received transaction:\n" + transaction.toString());
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction transaction, BigInteger arg2, BigInteger arg3) {
        System.out.println("SIMPLE. Wallet " + wallet.hashCode() + " send transaction:\n" + transaction.toString());
    }

    @Override
    public void onReorganize(Wallet wallet) {
        System.out.println("SIMPLE. Wallet " + wallet.hashCode() + " was reorganised");
    }

    public void onTransactionConfidenceChanged(Wallet wallet, Transaction transaction) {
        System.out.println("SIMPLE. Confidence changed for wallet: " + wallet.hashCode() + ", transaction:\n" + transaction.toString());
    }

    @Override
    public void displayView(int viewToDisplay) {
        System.out.println("SIMPLE. Displaying view : " + viewToDisplay);
    }

    @Override
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {
    }

    @Override
    public void fireDataChanged() {
        System.out.println("SIMPLE. Data has changed");
    }

    @Override
    public void recreateAllViews(boolean initUI) {
        System.out.println("SIMPLE. All views were recreated");
    }

    @Override
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        System.out.println("SIMPLE. Files have been changed by another process");
    }

    @Override
    public void nowOnline() {
        online = true;
        System.out.println("SIMPLE. Now online");
    }

    @Override
    public void nowOffline() {
        online = false;
        System.out.println("SIMPLE. Now offline");
    }

    @Override
    public void blockDownloaded() {
        numberOfBlocksDownloaded++;
    }

    @Override
    public void setHelpContext(String helpContextToDisplay) {
        System.out.println("SIMPLE. Help : " + helpContextToDisplay);
    }
}