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

import java.io.File;
import java.io.IOException;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Create new Wallet view
 * 
 * @author jim
 * 
 */
public class CreateNewWalletSubmitAction implements Action {

    private static final Logger log = LoggerFactory.getLogger(CreateNewWalletSubmitAction.class);

    private MultiBitController controller;

    public CreateNewWalletSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the file name from the data provider
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item item = data.getItem(MultiBitModel.ACTIVE_WALLET_FILENAME);
                if (item != null && item.getNewValue() != null) {

                    String newWalletFilename = (String) item.getNewValue();

                    if (new File(newWalletFilename).isDirectory()) {
                        return;
                    }

                    // if the filename has no extension, put on the wallet
                    // extension
                    if (!newWalletFilename.contains(".")) {
                        // add wallet file extension
                        newWalletFilename = newWalletFilename + "." + MultiBitModel.WALLET_FILE_EXTENSION;
                    }

                    File newWalletFile = new File(newWalletFilename);

                    try {
                        // if file exists, load the existing wallet
                        if (newWalletFile.exists()) {
                            PerWalletModelData perWalletModelData = controller.getFileHandler().loadFromFile(newWalletFile);
                            if (perWalletModelData != null) {
                                controller.getModel().setActiveWalletByFilename(perWalletModelData.getWalletFilename());
                                controller.fireNewWalletCreated();
                                controller.getModel().setUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");
                            }
                        } else {
                            // create a new wallet
                            Wallet newWallet = new Wallet(controller.getMultiBitService().getNetworkParameters());
                            ECKey newKey = new ECKey();
                            newWallet.keychain.add(newKey);
                            PerWalletModelData perWalletModelData = new PerWalletModelData();
                            perWalletModelData.setWalletInfo(new WalletInfo(newWalletFilename));
                            perWalletModelData.setWallet(newWallet);
                            perWalletModelData.setWalletFilename(newWalletFilename);
                            perWalletModelData.setWalletDescription(controller.getLocaliser().getString(
                                    "createNewWalletSubmitAction.defaultDescription"));
                            controller.getFileHandler().savePerWalletModelData(perWalletModelData, true);

                            // start using the new file as the wallet
                            controller.addWalletFromFilename(newWalletFile.getAbsolutePath());
                            controller.getModel().setActiveWalletByFilename(newWalletFilename);

                            controller.getModel().setUserPreference(MultiBitModel.GRAB_FOCUS_FOR_ACTIVE_WALLET, "true");
                                                    
                            // save the user properties to disk
                            FileHandler.writeUserPreferences(controller);
    
                            controller.fireNewWalletCreated();
                        }
                    } catch (IOException e) {
                        log.error("IOException: {}", e.getMessage(), e);
                    }
                }
            }
            controller.setActionForwardToParent();
        } else {
            // should never happen return to parent view
            controller.setActionForwardToParent();
        }
    }
}
