package org.multibit.network;

import com.google.bitcoin.store.BlockStoreException;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Queue;
import java.util.TimerTask;



/**
 * The timer task used to poll the replay task queue and initiate replays.
 * 
 */
public class ReplayManagerTimerTask extends TimerTask {
    private final Controller controller;
    private final BitcoinController bitcoinController;

    private static final Logger log = LoggerFactory.getLogger(ReplayManager.class);

    // State control booleans.
    private boolean currentlyRunningATask = false;
    private boolean currentTaskHasCompleted = false;
    private boolean currentTaskIsTidyingUp = false;

    final private Queue<ReplayTask> replayTaskQueue;

    public ReplayManagerTimerTask(BitcoinController bitcoinController, Queue<ReplayTask> replayTaskQueue) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.replayTaskQueue = replayTaskQueue;
    }

    @Override
    public void run() {
        synchronized (replayTaskQueue) {
            // If the current task has completed then it can be removed.
            if (currentTaskHasCompleted) {
                currentTaskHasCompleted = false;
                currentlyRunningATask = false;

                if (replayTaskQueue.peek() != null) {
                    // Remove that task from the queue.
                    log.debug("ReplayTask " + replayTaskQueue.peek().toString() + " has completed.");
                    replayTaskQueue.poll();
                }
            }

            // Start the new task if the current task is not running, not tidying up 
            // and there is another task waiting.
            if (!currentlyRunningATask && !currentTaskIsTidyingUp && replayTaskQueue.peek() != null) {
                try {
                    currentlyRunningATask = true;
                    ReplayManager.INSTANCE.syncWallet(replayTaskQueue.peek());
                } catch (IOException ioe) {
                    log.error(ioe.getClass().getCanonicalName() + " " + ioe.getMessage());
                } catch (BlockStoreException bse) {
                    log.error(bse.getClass().getCanonicalName() + " " + bse.getMessage());
                }
            }
        }
    }

    public void currentTaskHasCompleted() {
        currentTaskHasCompleted = true;
    }

    public void currentTaskIsTidyingUp(boolean currentTaskIsTidyingUp) {
        this.currentTaskIsTidyingUp = currentTaskIsTidyingUp;
    }

}
