package org.multibit.viewsystem.swing;

import java.util.TimerTask;

import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.core.MultiBitView;
import org.multibit.viewsystem.swing.view.panels.ShowTransactionsPanel;

public class UpdateTransactionsTimerTask extends TimerTask {
    private MultiBitController controller;
    private ShowTransactionsPanel transactionsPanel;
    private MultiBitFrame mainFrame;

    private Boolean updateTransactions = Boolean.FALSE;

    public UpdateTransactionsTimerTask(MultiBitController controller, final ShowTransactionsPanel transactionsPanel,
            MultiBitFrame mainFrame) {
        this.controller = controller;
        this.transactionsPanel = transactionsPanel;
        this.mainFrame = mainFrame;
    }

    @Override
    public void run() {
        // If viewing transactions, refresh the screen so that transaction
        // confidence icons can update.
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                boolean updateThisTime = false;
                if (updateTransactions) {
                    updateTransactions = false;
                    updateThisTime = true;
                }

                if (updateThisTime) {
                    mainFrame.updateHeader();
                    if (controller.getCurrentView() == MultiBitView.TRANSACTIONS_VIEW) {
                        // log.debug("Updating transaction view");
                        transactionsPanel.displayView();
                    }
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
