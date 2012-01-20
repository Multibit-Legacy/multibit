/**
 * Copyright 2012 multibit.org
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

import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Scanner;
import java.util.Set;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
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
public class ImportPrivateKeysSubmitAction implements Action {
    private static final String COMMENT_STRING_PREFIX = "#";

    private static final Logger log = LoggerFactory.getLogger(ImportPrivateKeysSubmitAction.class);

    private MultiBitController controller;

    private static final String SEPARATOR = " ";

    private SimpleDateFormat formatter;

    public ImportPrivateKeysSubmitAction(MultiBitController controller) {
        this.controller = controller;

        // date format is UTC with century, T time separator and Z for UTC
        // timezone
        formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    }

    public void execute(DataProvider dataProvider) {
        String message = controller.getLocaliser().getString("showImportPrivateKeysAction.noDataWasImported");

        // get the required output file
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item privateKeysOutputFilenameItem = data.getItem(MultiBitModel.PRIVATE_KEY_FILENAME);
                if (privateKeysOutputFilenameItem != null) {
                    String outputFilename = (String) privateKeysOutputFilenameItem.getNewValue();

                    if (outputFilename == null || outputFilename.equals("")) {
                        // no import file - nothing to do
                        message = controller.getLocaliser().getString("showImportPrivateKeysAction.privateKeysNothingToDo");
                    } else {

                        try {
                            Scanner scanner = new Scanner(new FileReader(outputFilename));
                            try {
                                // first use a Scanner to get each line
                                while (scanner.hasNextLine()) {
                                    processLine(scanner.nextLine());
                                }
                            } finally {
                                // ensure the underlying stream is always closed
                                // this only has any effect if the item passed
                                // to the Scanner
                                // constructor implements Closeable (which it
                                // does in this case).
                                scanner.close();
                            }

                            // parse keys

                            // add to wallet

                            // begin blockchain replay

                            // message =
                            // controller.getLocaliser().getString("showImportPrivateKeysAction.privateKeysImportSuccess");

                            message = "Still a bit to do yet!";

                        } catch (Exception e) {// Catch exception if any
                            log.error(e.getClass().getName() + " " + e.getMessage());
                            message = controller.getLocaliser().getString("showImportPrivateKeysAction.privateKeysImportFailure",
                                    new Object[] { e.getClass().getName() + " " + e.getMessage() });
                        }
                    }
                }
            }
        }

        controller.getModel().setUserPreference(MultiBitModel.DISPLAY_IMPORT_PRIVATE_KEYS_MESSAGE, "true");
        controller.getModel().setUserPreference(MultiBitModel.IMPORT_PRIVATE_KEYS_MESSAGE, message);
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);
    }

    protected void processLine(String aLine) {
        if (aLine == null || aLine.trim().equals("") || aLine.startsWith(COMMENT_STRING_PREFIX)) {
            // nothing to do
        } else {
            Scanner scanner = null;
            try {
                scanner = new Scanner(aLine);

                String sipaKey = "";
                String createdAtAsString = "";
                if (scanner.hasNext()) {
                    sipaKey = scanner.next();
                    
                }
                if (scanner.hasNext()) {
                    createdAtAsString = scanner.next();
                }
                System.out.println("ImportPrivateKeysSubmitAction sipaKey= '" + sipaKey + "',  createdAt = '" + createdAtAsString
                        + "'");
            } finally {
                if (scanner != null) {
                    scanner.close();
                }
            }
        }
    }
}
