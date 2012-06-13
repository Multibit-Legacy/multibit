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
package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;
import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import javax.swing.AbstractAction;
import javax.swing.Action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletInfo;
import org.multibit.viewsystem.dataproviders.CreateBulkAddressesDataProvider;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.ECKey;

/**
 * This {@link Action} creates a file with bulk addresses in for use by
 * MultiBitMerchant
 */
public class CreateBulkAddressesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private CreateBulkAddressesDataProvider dataProvider;

    /**
     * Creates a new {@link CreateBulkAddressesSubmitAction}.
     */
    public CreateBulkAddressesSubmitAction(MultiBitController controller, CreateBulkAddressesDataProvider dataProvider) {
        super(controller.getLocaliser().getString("createBulkAddressesSubmitAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("createBulkAddressesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("createBulkAddressesSubmitAction.mnemonicKey"));
    }

    /**
     * create output file with addresses
     */
    public void actionPerformed(ActionEvent e) {
        // check to see if the wallet files have changed
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            if (dataProvider != null) {
                String outputFilename = dataProvider.getOutputFilename();
                int numberOfAddresses = dataProvider.getNumberOfAddresses();

                WalletInfo walletInfo = perWalletModelData.getWalletInfo();

                if (outputFilename != null && numberOfAddresses > 0) {
                    // write the bulk addresses
                    OutputStream outputStream = null;
                    BufferedOutputStream bufferedOutputStream = null;
                    OutputStreamWriter outputStreamWriter = null;
                    try {
                        outputStream = new FileOutputStream(outputFilename);
                        bufferedOutputStream = new BufferedOutputStream(outputStream);
                        outputStreamWriter = new OutputStreamWriter(bufferedOutputStream, "UTF8");

                        String addressString = null;
                        for (int i = 0; i < numberOfAddresses; i++) {
                            ECKey key = new ECKey();
                            perWalletModelData.getWallet().keychain.add(key);
                            Address address = key.toAddress(controller.getMultiBitService().getNetworkParameters());
                            addressString = address.toString();
                            outputStreamWriter.write(addressString + "\n");

                            walletInfo.addReceivingAddress(new AddressBookData("", addressString), false);
                        }
                        // put last address in wallet preferences just for UI consistency
                        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_ADDRESS, addressString);
                        controller.getModel().setActiveWalletPreference(MultiBitModel.RECEIVE_LABEL, "");

                        controller.getFileHandler().savePerWalletModelData(perWalletModelData, false);
                    } catch (FileNotFoundException fnfe) {
                        fnfe.printStackTrace();
                    } catch (IOException ioe) {
                        ioe.printStackTrace();
                    } finally {
                        if (outputStreamWriter != null) {
                            try {
                                outputStreamWriter.flush();
                            } catch (IOException ioe) {
                                ioe.printStackTrace();
                            }
                        }
                        if (bufferedOutputStream != null) {
                            try {
                                bufferedOutputStream.close();
                            } catch (IOException ioe2) {
                                ioe2.printStackTrace();
                            }
                        }
                        if (outputStream != null) {
                            try {
                                outputStream.close();
                            } catch (IOException ioe3) {
                                ioe3.printStackTrace();
                            }
                        }
                    }
                }
            }
        }

        // return to same view
        controller.displayView(controller.getCurrentView());
    }
}