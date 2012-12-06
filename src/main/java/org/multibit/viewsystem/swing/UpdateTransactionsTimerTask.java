package org.multibit.viewsystem.swing;

import java.util.TimerTask;

import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.view.ShowTransactionsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateTransactionsTimerTask extends TimerTask {

    private static final Logger log = LoggerFactory.getLogger(UpdateTransactionsTimerTask.class);

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
                        //log.debug("Updating transaction view");
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
