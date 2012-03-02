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
package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JPasswordField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

import org.multibit.controller.MultiBitController;
import org.multibit.crypto.EncrypterDecrypterException;
import org.multibit.file.PrivateKeyAndDate;
import org.multibit.file.PrivateKeysHandler;
import org.multibit.file.PrivateKeysHandlerException;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.view.ImportPrivateKeysPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.store.BlockStoreException;

/**
 * This {@link Action} imports the private keys to the active wallet
 */
public class ImportPrivateKeysSubmitAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(ImportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923492087598757765L;

    private MultiBitController controller;
    private ImportPrivateKeysPanel importPrivateKeysPanel;
    private JPasswordField passwordField;

    private static final long BUTTON_DOWNCLICK_TIME = 400;

    /**
     * Creates a new {@link ImportPrivateKeysSubmitAction}.
     */
    public ImportPrivateKeysSubmitAction(MultiBitController controller, ImportPrivateKeysPanel importPrivateKeysPanel,
            ImageIcon icon, JPasswordField passwordField) {
        super(controller.getLocaliser().getString("importPrivateKeysSubmitAction.text"), icon);
        this.controller = controller;
        this.importPrivateKeysPanel = importPrivateKeysPanel;
        this.passwordField = passwordField;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("importPrivateKeysSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("importPrivateKeysSubmitAction.mnemonicKey"));
    }

    /**
     * import the private keys and replay the blockchain
     */
    public void actionPerformed(ActionEvent event) {
        // check to see if another process has changed the active wallet
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = controller.getFileHandler().haveFilesChanged(perWalletModelData);

        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            controller.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData);
        } else {
            final ImportPrivateKeysSubmitAction thisAction = this;

            String importFilename = importPrivateKeysPanel.getOutputFilename();
            if (importFilename == null || importFilename.equals("")) {
                // no import file - nothing to do
                importPrivateKeysPanel.setMessage(controller.getLocaliser().getString(
                        "importPrivateKeysSubmitAction.privateKeysNothingToDo"));
            } else {
                setEnabled(false);
                importPrivateKeysPanel.setMessage(controller.getLocaliser().getString(
                        "importPrivateKeysSubmitAction.importingPrivateKeys"));

                File importFile = new File(importFilename);

                importPrivateKeysInBackground(importFile);

                Timer timer = new Timer();
                timer.schedule(new TimerTask() {
                    @Override
                    public void run() {
                        thisAction.setEnabled(true);
                    }
                }, BUTTON_DOWNCLICK_TIME);
            }
        }
    }

    /**
     * import the private keys in a background Swing worker thread
     */
    private void importPrivateKeysInBackground(File importFile) {
        final PerWalletModelData finalPerWalletModelData = controller.getModel().getActivePerWalletModelData();
        final File finalImportFile = importFile;
        final ImportPrivateKeysPanel finalThisPanel = importPrivateKeysPanel;
        final MultiBitController finalController = controller;
        final char[] finalPassword = passwordField.getPassword();

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            private String statusBarMessage = null;
            private String uiMessage = null;

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;

                try {
                    PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getMultiBitService()
                            .getNetworkParameters());

                    Collection<PrivateKeyAndDate> privateKeyAndDateArray = privateKeysHandler.importPrivateKeys(finalImportFile, finalPassword);

                    // add to wallet and keep track of earliest transaction date
                    // go backwards from now
                    Wallet walletToAddKeysTo = finalPerWalletModelData.getWallet();
                    Date earliestTransactionDate = new Date();
                    if (privateKeyAndDateArray != null) {
                        for (PrivateKeyAndDate privateKeyAndDate : privateKeyAndDateArray) {
                            ECKey keyToAdd = privateKeyAndDate.getKey();
                            if (keyToAdd != null) {
                                if (walletToAddKeysTo != null
                                        && !keyChainContainsPrivateKey(walletToAddKeysTo.getKeychain(), keyToAdd)) {
                                    walletToAddKeysTo.addKey(keyToAdd);

                                    // update earliest transaction date
                                    if (privateKeyAndDate.getDate() == null) {
                                        // need to go back to the genesis block
                                        earliestTransactionDate = null;
                                    } else {
                                        if (earliestTransactionDate != null) {
                                            earliestTransactionDate = earliestTransactionDate.before(privateKeyAndDate.getDate()) ? earliestTransactionDate
                                                    : privateKeyAndDate.getDate();
                                        }
                                    }
                                }
                            }

                        }
                    }
                    controller.getFileHandler().savePerWalletModelData(finalPerWalletModelData, false);
                    controller.getModel().createAddressBookReceivingAddresses(finalPerWalletModelData.getWalletFilename());
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            if (finalThisPanel != null) {
                                finalThisPanel.setMessage(finalController.getLocaliser().getString(
                                        "importPrivateKeysSubmitAction.privateKeysImportSuccess"));
                            }
                        }
                    });

                    // begin blockchain replay - returns quickly - just kicks it off
                    controller.getMultiBitService().replayBlockChain(earliestTransactionDate);
                    successMeasure = Boolean.TRUE;
                    statusBarMessage = controller.getLocaliser().getString("resetTransactionsSubmitAction.startReplay");
                } catch (EncrypterDecrypterException ede) {
                    log.error(ede.getClass().getName() + " " + ede.getMessage());
                    uiMessage = controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { ede.getClass().getName() + " " + ede.getMessage() });

                } catch (PrivateKeysHandlerException pkhe) {
                    log.error(pkhe.getClass().getName() + " " + pkhe.getMessage());
                    uiMessage = controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { pkhe.getClass().getName() + " " + pkhe.getMessage() });

                } catch (BlockStoreException bse) {
                    log.error(bse.getClass().getName() + " " + bse.getMessage());
                    statusBarMessage = controller.getLocaliser().getString("resetTransactionsSubmitAction.replayUnsuccessful",
                            new Object[] { bse.getMessage() });
                    uiMessage = controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { bse.getClass().getName() + " " + bse.getMessage() });
                }
                return successMeasure;
            }

            protected void done() {
                try {
                    Boolean wasSuccessful = get();

                    if (finalThisPanel != null && uiMessage != null) {
                        finalThisPanel.setMessage(uiMessage);
                    }

                    controller.updateStatusLabel(statusBarMessage);

                    if (wasSuccessful) {
                        log.debug(statusBarMessage);
                    } else {
                        log.error(statusBarMessage);
                    }
                } catch (Exception e) {
                    // not really used but caught so that SwingWorker shuts down cleanly
                    log.error(e.getClass() + " " + e.getMessage());
                }
            }
        };
        log.debug("Importing private keys in background SwingWorker thread");
        worker.execute();
    }

    /**
     * this method is here because there is no equals on ECKey
     */
    private boolean keyChainContainsPrivateKey(ArrayList<ECKey> keyChain, ECKey keyToAdd) {
        if (keyChain == null || keyToAdd == null) {
            return false;
        } else {
            for (ECKey loopKey : keyChain) {
                if (Arrays.equals(keyToAdd.getPrivKeyBytes(), loopKey.getPrivKeyBytes())) {
                    return true;
                }
            }
            return false;
        }
    }
}