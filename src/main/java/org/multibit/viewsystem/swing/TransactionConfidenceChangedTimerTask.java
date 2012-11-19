package org.multibit.viewsystem.swing;

import java.util.TimerTask;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.view.ViewFactory;

public class TransactionConfidenceChangedTimerTask extends TimerTask {

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    @SuppressWarnings("unused")
    private Boolean isTransactionConfidenceChangedTimerRunning;
    private ViewFactory viewFactory;
    
    public TransactionConfidenceChangedTimerTask(MultiBitController controller, MultiBitFrame mainFrame, Boolean isTransactionConfidenceChangedTimerRunning, ViewFactory viewFactory) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.isTransactionConfidenceChangedTimerRunning = isTransactionConfidenceChangedTimerRunning;
        this.viewFactory = viewFactory;
    }
    
    @Override
    public void run() {
        // If viewing transactions, refresh the screen so that transaction confidence icons can update.
        if (controller.getCurrentView() == View.TRANSACTIONS_VIEW) {
            View currentViewView = viewFactory.getView(controller.getCurrentView());
            if (currentViewView != null) {
                currentViewView.displayView();
                
                mainFrame.invalidate();
                mainFrame.validate();
                mainFrame.repaint();
            }
        }
        isTransactionConfidenceChangedTimerRunning = Boolean.FALSE;
    }
}
