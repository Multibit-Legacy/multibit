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
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.DumpedPrivateKey;
import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.ScriptException;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.TransactionInput;
import com.google.bitcoin.core.TransactionOutput;
import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Export Private Keys submit action
 * 
 * @author jim
 * 
 */
public class ExportPrivateKeysSubmitAction implements Action {
    private static final Logger log = LoggerFactory.getLogger(ExportPrivateKeysSubmitAction.class);

    private MultiBitController controller;
    
    private PrivateKeysHandler privateKeysHandler;

    public ExportPrivateKeysSubmitAction(MultiBitController controller) {
        this.controller = controller;
        
        privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService().getNetworkParameters());
    }

    public void execute(DataProvider dataProvider) {
        String message = controller.getLocaliser().getString("showExportPrivateKeysAction.noDataWasWritten");

        // get the required output file
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item privateKeysOutputFilenameItem = data.getItem(MultiBitModel.PRIVATE_KEY_FILENAME);
                if (privateKeysOutputFilenameItem != null) {
                    String outputFilename = (String) privateKeysOutputFilenameItem.getNewValue();
                    // TODO check to see if file already exists - do not
                    // overwrite

                    try {
                        privateKeysHandler.exportPrivateKeys(new File(outputFilename),  controller.getModel().getActivePerWalletModelData().getWallet());
                      
                        // success
                        message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportSuccess");
                    } catch (IOException ioe) {
                        log.error(ioe.getClass().getName() + " " + ioe.getMessage());
                        //failure
                        message = controller.getLocaliser().getString("showExportPrivateKeysAction.privateKeysExportFailure",
                                new Object[] { ioe.getClass().getName() + " " + ioe.getMessage() });
                    }
                }
            }
        }

        controller.getModel().setUserPreference(MultiBitModel.DISPLAY_EXPORT_PRIVATE_KEYS_MESSAGE, "true");
        controller.getModel().setUserPreference(MultiBitModel.EXPORT_PRIVATE_KEYS_MESSAGE, message);
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }
}
