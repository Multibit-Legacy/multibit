package org.multibit.viewsystem.swing;

import java.util.TimerTask;

import javax.swing.SwingUtilities;

import org.multibit.viewsystem.DisplayHint;

/**
 * Condense the many fire data change events into something more manageable for a UI to refresh.
 * @author jim
 *
 */
public class FireDataChangedTimerTask extends TimerTask {
    private MultiBitFrame mainFrame;

    private boolean fireDataChanged = false;
    private boolean isCurrentlyUpdating = false;

    public FireDataChangedTimerTask(MultiBitFrame mainFrame) {
        this.mainFrame = mainFrame;
    }

    @Override
    public void run() {
        // If still updating from the last time, skip this firing.
        // Timer thread.
        if (!isCurrentlyUpdating) {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    boolean fireDataChangedThisTime = false;
                    if (fireDataChanged) {
                        fireDataChanged = false;
                        fireDataChangedThisTime = true;
                    }

                    if (fireDataChangedThisTime) {
                        // Swing thread.
                        isCurrentlyUpdating = true;
                        try {
                            mainFrame.fireDataChangedUpdateNow(DisplayHint.WALLET_TRANSACTIONS_HAVE_CHANGED);
                        } finally {
                            // Swing thread.
                            isCurrentlyUpdating = false;
                        }
                    }

                }
            });
        }
    }

    public boolean isFireDataChanged() {
        return fireDataChanged;
    }

    public void setFireDataChanged (boolean fireDataChanged) {
        this.fireDataChanged = fireDataChanged;
    }
}
