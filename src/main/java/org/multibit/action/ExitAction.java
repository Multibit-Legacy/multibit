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
package org.multibit.action;

import java.awt.Cursor;
import java.util.List;

import javax.swing.SwingWorker;

import org.multibit.ApplicationInstanceManager;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * exit the application
 * 
 * @author jim
 * 
 */
public class ExitAction implements Action {
    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private static final Logger log = LoggerFactory.getLogger(ExitAction.class);

    public ExitAction(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
    }

    public void execute(DataProvider dataProvider) {
        if (mainFrame != null) {
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }
        // save all the wallets and put their filenames in the user preferences
        if (controller.getModel().getPerWalletModelDataList() != null) {
            List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            int numberOfWallets = perWalletModelDataList.size();
            if (numberOfWallets > 0) {
                for (int i = 1; i <= numberOfWallets; i++) {
                    PerWalletModelData perWalletModelData = perWalletModelDataList.get(i - 1);
                    if (perWalletModelData.getWalletFilename() != null) {
                        controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
                    }
                }
            }
        }
        // write the user properties
        log.debug("Saving user preferences ...");
        //controller.updateStatusLabel("Saving user preferences ...");
        controller.getFileHandler().writeUserPreferences();

        log.debug("Shutting down Bitcoin URI checker ...");
        ApplicationInstanceManager.shutdownSocket();

        // get rid of main display
        if (mainFrame != null) {
            mainFrame.setVisible(false);
        }
        
        if (controller.getMultiBitService() != null && controller.getMultiBitService().getPeerGroup() != null) {
            log.debug("Closing Bitcoin network connection...");
            //controller.updateStatusLabel("Closing Bitcoin network connection...");
            @SuppressWarnings("rawtypes")
            SwingWorker worker = new SwingWorker() {
                @Override
                protected Object doInBackground() throws Exception {
                    controller.getMultiBitService().getPeerGroup().stop();
                    return null; // return not used
                }
            };
            worker.execute();
        }

        if (mainFrame != null) {
            mainFrame.dispose();
        }
        
        System.exit(0);
    }
}
