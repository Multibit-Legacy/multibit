/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.viewsystem.swing;

import java.util.List;
import java.util.TimerTask;

import org.multibit.controller.MultiBitController;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.FileHandler;

/**
 * TimerTask to detect whether wallet files have been changed by some external
 * process
 * 
 * @see java.util.Timer
 * @see java.util.TimerTask
 */
public class FileChangeTimerTask extends TimerTask {

    private final MultiBitController controller;
    private final MultiBitFrame mainFrame;
    private final FileHandler fileHandler;

    /**
     * Constructs the object, sets the string to be output in function run()
     * 
     * @param str
     */
    public FileChangeTimerTask(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        fileHandler = new FileHandler(controller);
    }

    /**
     * When the timer executes, this code is run.
     */
    public void run() {
        // see if the wallet files have changed
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

        if (perWalletModelDataList != null) {
            for (PerWalletModelData loopModelData : perWalletModelDataList) {
                boolean haveFilesChanged = fileHandler.haveFilesChanged(loopModelData);
                if (haveFilesChanged) {
                    boolean previousFilesHaveBeenChanged = loopModelData.isFilesHaveBeenChangedByAnotherProcess();
                    loopModelData.setFilesHaveBeenChangedByAnotherProcess(true);
                    if (!previousFilesHaveBeenChanged) {
                        // only fire once, when change happens
                        controller.fireFilesHaveBeenChangedByAnotherProcess(loopModelData);
                    }
                }
            }
        }

        // refresh the main screen
        mainFrame.fireDataChanged();
    }
}