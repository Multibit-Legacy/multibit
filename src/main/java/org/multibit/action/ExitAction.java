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

import java.util.List;

import org.multibit.ApplicationInstanceManager;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;

/**
 * exit the application
 * 
 * @author jim
 * 
 */
public class ExitAction implements Action {
    private MultiBitController controller;

    public ExitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // write the user properties
        // save all the wallets and put their filenames in the user preferences
        if (controller.getModel().getPerWalletModelDataList() != null) {
            List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            int numberOfWallets = perWalletModelDataList.size();
            controller.getModel().setUserPreference(MultiBitModel.NUMBER_OF_WALLETS, numberOfWallets + "");
            controller.getModel().setUserPreference(MultiBitModel.ACTIVE_WALLET_FILENAME,
                    controller.getModel().getActiveWalletFilename());
            if (numberOfWallets > 0) {
                for (int i = 1; i <= numberOfWallets; i++) {
                    PerWalletModelData perWalletModelData = perWalletModelDataList.get(i - 1);
                    if (perWalletModelData.getWalletFilename() != null) {
                        controller.getModel().setUserPreference(MultiBitModel.WALLET_FILENAME_PREFIX + i,
                                perWalletModelData.getWalletFilename());
                        // save the ith wallet, including the wallet info
                        controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
                    }
                }

            }

        }

        controller.getFileHandler().writeUserPreferences();

        // shut down the PeerGroup
        if (controller.getMultiBitService() != null && controller.getMultiBitService().getPeerGroup() != null) {
            controller.getMultiBitService().getPeerGroup().stop();
        }

        ApplicationInstanceManager.shutdownSocket();
        
        System.exit(0);
    }
}
