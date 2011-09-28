package org.multibit.viewsystem.swing;

import java.util.TimerTask;


/**
 * This is a timertask because it extends the class java.util.TimerTask. This
 * class will be given to the timer (java.util.Timer) as the code to be
 * executed.
 * 
 * @see java.util.Timer
 * @see java.util.TimerTask
 * @author http://www.gammelsaeter.com/
 */
public class RefreshTimerTask extends TimerTask {

    private MultiBitFrame mainFrame;

    /**
     * Constructs the object, sets the string to be output in function run()
     * 
     * @param str
     */
    public RefreshTimerTask(MultiBitFrame mainFrame) {
        this.mainFrame = mainFrame;
    }

    /**
     * When the timer executes, this code is run.
     */
    public void run() {
        // refresh the main screen
        mainFrame.fireDataChanged();
    }
}