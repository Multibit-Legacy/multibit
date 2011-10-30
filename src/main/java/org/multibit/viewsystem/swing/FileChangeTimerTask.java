package org.multibit.viewsystem.swing;

import java.util.List;
import java.util.TimerTask;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.FileHandler;


/**
 * TimerTask to detect whether wallet files have been changed by some external process
 * 
 * @see java.util.Timer
 * @see java.util.TimerTask
 */
public class FileChangeTimerTask extends TimerTask {

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    /**
     * Constructs the object, sets the string to be output in function run()
     * 
     * @param str
     */
    public FileChangeTimerTask(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }

    /**
     * When the timer executes, this code is run.
     */
    public void run() {
        // see if the wallet files have changed
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        
        FileHandler fileHandler = new FileHandler(controller);
        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopModelData : perWalletModelDataList) {
                boolean haveFilesChanged = fileHandler.haveFilesChanged(loopModelData);
                if (haveFilesChanged) {
                    loopModelData.setFilesHaveBeenChangedByAnotherProcess(true);
                }
            }
        }
        
        // refresh the main screen
        mainFrame.fireDataChanged();
    }
}