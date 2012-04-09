package org.multibit.viewsystem.commandline;

import java.io.InputStream;
import java.io.PrintStream;
import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.ViewSystem;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;

/**
 * a purely text / command line view for MultiBit
 * 
 * @author jim
 * 
 */
public class CommandLineViewSystem implements ViewSystem {

    private MultiBitController controller;

    private InputStream inputStream;
    private PrintStream printStream;

    private boolean alreadySaidIsOnline = false;

    public static String TEXT_VIEW_OUTPUT_PREFIX = "MB:";
    public static String SEPARATOR = " ";
    public static String MESSAGE_PREFIX = "Message   : ";
    public static String DISPLAY_VIEW_PREFIX = "Displaying: ";
    public static String NAVIGATE_AWAY_FROM_VIEW_PREFIX = "Leaving   : ";
    public static String ERROR_PREFIX = "Error     : ";
    public static String DISPLAY_MENU_OPTIONS_PREFIX = "Menu      : ";
    public static String MENU_CHOOSE_PREFIX = "CHOOSE    > ";
    
    private MultiBitShell multiBitShell;

    public CommandLineViewSystem(MultiBitController controller) {
        this.controller = controller;

        inputStream = System.in;
        printStream = System.out;

        multiBitShell = new MultiBitShell(controller, printStream);
        ConsoleReadingThread consoleReader = new ConsoleReadingThread(System.in, System.out, multiBitShell);
        consoleReader.start();
    }

    public void displayView(int viewId) {
            //printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX + "No view to display with id " + viewId);
    }

    public void navigateAwayFromView(int viewToNavigateAwayFrom, int nextViewId, int relationshipOfNewViewToPrevious) {
            //printStream.println(TEXT_VIEW_OUTPUT_PREFIX + ERROR_PREFIX + "No view to navigate away from.   Next view id is "
            //        + nextViewId);
    }

    /**
     * command line does not have views
     */
    public void recreateAllViews() {
        // nothing to do
    }

    public void nowOnline() {
        if (!alreadySaidIsOnline) {
            printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "MultiBit is now online");
        }
        alreadySaidIsOnline = true;
    }

    public void nowOffline() {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "MultiBit is now online");
        alreadySaidIsOnline = false;
    }

    public void updateStatusLabel(String updateDownloadStatus) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus);
    }

    public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {

        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "onCoinsReceived, previous balance = " + prevBalance
                + ", new balance = " + newBalance);
    }

    public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance, BigInteger newBalance) {

        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + "onPendingCoinsReceived, previous balance = " + prevBalance
                + ", new balance = " + newBalance);
    }

    public void blockDownloaded() {
        // do nothing
    }

    @Override
    public void onCoinsSent(Wallet wallet, Transaction tx, BigInteger prevBalance, BigInteger newBalance) {
        // TODO Auto-generated method stub

    }

    @Override
    public void onReorganize(Wallet wallet) {
        // TODO Auto-generated method stub

    }

    @Override
    public void onTransactionConfidenceChanged(Wallet wallet, Transaction tx) {
        // TODO Auto-generated method stub

    }

    @Override
    public void navigateAwayFromView(int viewToNavigateAwayFrom) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireDataChanged() {
        // TODO Auto-generated method stub

    }

    @Override
    public void recreateAllViews(boolean initUI) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fireFilesHaveBeenChangedByAnotherProcess(PerWalletModelData perWalletModelData) {
        // TODO Auto-generated method stub

    }

    @Override
    public void updateStatusLabel(String updateDownloadStatus, boolean clearAutomatically) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus);
    }

    @Override
    public void updateStatusLabel(String updateDownloadStatus, double percentComplete) {
        printStream.println(TEXT_VIEW_OUTPUT_PREFIX + MESSAGE_PREFIX + updateDownloadStatus + ". " + percentComplete
                + "% complete.");

    }

    @Override
    public void setHelpContext(String helpContextToDisplay) {
        // TODO Auto-generated method stub

    }

    public MultiBitShell getMultiBitShell() {
        return multiBitShell;
    }
}
