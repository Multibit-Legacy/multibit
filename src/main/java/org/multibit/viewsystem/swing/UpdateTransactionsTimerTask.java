package org.multibit.viewsystem.swing;

import java.util.TimerTask;

import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.view.ShowTransactionsPanel;

public class UpdateTransactionsTimerTask extends TimerTask {

    private MultiBitController controller;
    private ShowTransactionsPanel transactionsPanel;
    private MultiBitFrame mainFrame;

    private final TimerTask thisTimerTask;

    private Boolean updateTransactions = Boolean.FALSE;

    public UpdateTransactionsTimerTask(MultiBitController controller, final ShowTransactionsPanel transactionsPanel,
            MultiBitFrame mainFrame) {
        this.controller = controller;
        this.transactionsPanel = transactionsPanel;
        this.mainFrame = mainFrame;
        thisTimerTask = this;

    }

    @Override
    public void run() {
        // If viewing transactions, refresh the screen so that transaction
        // confidence icons can update.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                boolean updateThisTime = false;
                synchronized (thisTimerTask) {
                    if (updateTransactions) {
                        updateTransactions = false;
                        updateThisTime = true;
                    }
                }

                if (updateThisTime) {
                    mainFrame.updateHeader();
                    if (controller.getCurrentView() == View.TRANSACTIONS_VIEW) {
                        transactionsPanel.displayView();
                    }
                    mainFrame.invalidate();
                    mainFrame.validate();
                }
            }
        });
    }

    public boolean isUpdateTransactions() {
        return updateTransactions;
    }

    public void setUpdateTransactions(boolean updateTransactions) {
        this.updateTransactions = updateTransactions;
    }
}
