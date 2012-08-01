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
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.utils.DateUtils;
import org.multibit.viewsystem.swing.view.ImportPrivateKeysPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletType;
import com.google.bitcoin.store.BlockStoreException;
import com.piuk.blockchain.MyWallet;

/**
 * This {@link Action} imports the private keys to the active wallet.
 */
public class ImportPrivateKeysSubmitAction extends MultiBitSubmitAction {

    private static final Logger log = LoggerFactory.getLogger(ImportPrivateKeysSubmitAction.class);

    private static final long serialVersionUID = 1923492087598757765L;

    private ImportPrivateKeysPanel importPrivateKeysPanel;
    private JPasswordField walletPasswordField;
    private JPasswordField passwordField;
    private JPasswordField passwordField2;

    private boolean performReplay = true;

    private static final long BUTTON_DOWNCLICK_TIME = 400;

    private static final long NUMBER_OF_MILLISECONDS_IN_A_SECOND = 1000;

    /**
     * Creates a new {@link ImportPrivateKeysSubmitAction}.
     */
    public ImportPrivateKeysSubmitAction(MultiBitController controller, ImportPrivateKeysPanel importPrivateKeysPanel,
            ImageIcon icon, JPasswordField walletPasswordField, JPasswordField passwordField1, JPasswordField passwordField2) {
        super(controller, "importPrivateKeysSubmitAction.text", "importPrivateKeysSubmitAction.tooltip",
                "importPrivateKeysSubmitAction.mnemonicKey", icon);
        this.importPrivateKeysPanel = importPrivateKeysPanel;
        this.walletPasswordField = walletPasswordField;
        this.passwordField = passwordField1;
        this.passwordField2 = passwordField2;
    }

    /**
     * Import the private keys and replay the blockchain.
     */
    public void actionPerformed(ActionEvent event) {
        if (abort()) {
            return;
        }

        final ImportPrivateKeysSubmitAction thisAction = this;

        String importFilename = importPrivateKeysPanel.getOutputFilename();
        if (importFilename == null || importFilename.equals("")) {
            // No import file - nothing to do.
            importPrivateKeysPanel.setMessageText(controller.getLocaliser().getString(
                    "importPrivateKeysSubmitAction.privateKeysNothingToDo"));
            return;
        }

        // See if a wallet password is required and present.
        if (controller.getModel().getActiveWallet() != null
                && controller.getModel().getActiveWallet().getWalletType() == WalletType.ENCRYPTED && controller.getModel().getActiveWallet().isCurrentlyEncrypted()) {
            if (walletPasswordField.getPassword() == null || walletPasswordField.getPassword().length == 0) {
                importPrivateKeysPanel.setMessageText(controller.getLocaliser().getString(
                        "showExportPrivateKeysAction.youMustEnterTheWalletPassword"));
                return;
            }

            // See if the password is the correct wallet password.
            if (!controller.getModel().getActiveWallet().checkPasswordCanDecryptFirstPrivateKey(walletPasswordField.getPassword())) {
                // The password supplied is incorrect.
                importPrivateKeysPanel.setMessageText(controller.getLocaliser().getString(
                        "createNewReceivingAddressSubmitAction.passwordIsIncorrect"));
                return;
            }
        }

        setEnabled(false);
        importPrivateKeysPanel.setMessageText(controller.getLocaliser().getString(
                "importPrivateKeysSubmitAction.importingPrivateKeys"));

        File importFile = new File(importFilename);

        char[] passwordChar = passwordField.getPassword();

        try {
            if (importPrivateKeysPanel.multiBitFileChooser.accept(importFile)) {

                PrivateKeysHandler privateKeysHandler = new PrivateKeysHandler(controller.getModel().getNetworkParameters());

                Collection<PrivateKeyAndDate> privateKeyAndDateArray = privateKeysHandler.readInPrivateKeys(importFile,
                        passwordChar);

                importPrivateKeysInBackground(privateKeyAndDateArray, walletPasswordField.getPassword());
            } else if (importPrivateKeysPanel.myWalletEncryptedFileChooser.accept(importFile)) {
                String importFileContents = PrivateKeysHandler.readFile(importFile);

                String mainPassword = new String(passwordField.getPassword());
                String secondPassword = new String(passwordField2.getPassword());

                MyWallet wallet = new MyWallet(importFileContents, mainPassword);

                boolean needSecondPassword = false;
                if (wallet.isDoubleEncrypted()) {
                    if ("".equals(secondPassword)) {
                        needSecondPassword = true;
                        importPrivateKeysPanel.requestSecondPassword();
                    }
                }

                if (!needSecondPassword) {
                    wallet.setTemporySecondPassword(secondPassword);

                    Wallet bitcoinj = wallet.getBitcoinJWallet();
                    Collection<PrivateKeyAndDate> privateKeyAndDateArray = new ArrayList<PrivateKeyAndDate>();
                    for (ECKey key : bitcoinj.keychain) {
                        privateKeyAndDateArray.add(new PrivateKeyAndDate(key, null));
                    }
                    importPrivateKeysInBackground(privateKeyAndDateArray, walletPasswordField.getPassword());
                }

            } else if (importPrivateKeysPanel.myWalletPlainFileChooser.accept(importFile)) {
                String importFileContents = PrivateKeysHandler.readFile(importFile);

                MyWallet wallet = new MyWallet(importFileContents);

                Wallet bitcoinj = wallet.getBitcoinJWallet();
                Collection<PrivateKeyAndDate> privateKeyAndDateArray = new ArrayList<PrivateKeyAndDate>();
                for (ECKey key : bitcoinj.keychain) {
                    privateKeyAndDateArray.add(new PrivateKeyAndDate(key, null));
                }
                importPrivateKeysInBackground(privateKeyAndDateArray, walletPasswordField.getPassword());

            }
        } catch (Exception e) {
            log.error(e.getClass().getName() + " " + e.getMessage());
            setEnabled(true);

            importPrivateKeysPanel.setMessageText(controller.getLocaliser().getString(
                    "importPrivateKeysSubmitAction.privateKeysUnlockFailure", new Object[] { e.getMessage() }));

            return;
        }

        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                thisAction.setEnabled(true);
            }
        }, BUTTON_DOWNCLICK_TIME);
    }

    /**
     * Import the private keys in a background Swing worker thread.
     */
    private void importPrivateKeysInBackground(final Collection<PrivateKeyAndDate> privateKeyAndDateArray,
            final char[] walletPassword) {
        final PerWalletModelData finalPerWalletModelData = controller.getModel().getActivePerWalletModelData();
        final ImportPrivateKeysPanel finalImportPanel = importPrivateKeysPanel;
        final MultiBitController finalController = controller;

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            private String statusBarMessage = null;
            private String uiMessage = null;

            @Override
            protected Boolean doInBackground() throws Exception {
                Boolean successMeasure = Boolean.FALSE;

                boolean reencryptionRequired = false;
                try {
                    // Decrypt the wallet before adding keys.
                    if (finalPerWalletModelData.getWallet().getWalletType() == WalletType.ENCRYPTED) {
                        if (finalPerWalletModelData.getWallet().isCurrentlyEncrypted()) {
                            finalPerWalletModelData.getWallet().decrypt(walletPassword);
                            reencryptionRequired = true;
                        }
                    }

                    // Keep track of earliest transaction date go backwards from now.
                    Wallet walletToAddKeysTo = finalPerWalletModelData.getWallet();
                    Date earliestTransactionDate = new Date(DateUtils.nowUtc().getMillis());
                    if (privateKeyAndDateArray != null) {
                        for (PrivateKeyAndDate privateKeyAndDate : privateKeyAndDateArray) {
                            ECKey keyToAdd = privateKeyAndDate.getKey();
                            if (keyToAdd != null) {
                                if (privateKeyAndDate.getDate() != null) {
                                    keyToAdd.setCreationTimeSeconds(privateKeyAndDate.getDate().getTime()
                                            / NUMBER_OF_MILLISECONDS_IN_A_SECOND);
                                }

                                if (walletToAddKeysTo != null
                                        && !keyChainContainsPrivateKey(walletToAddKeysTo.getKeychain(), keyToAdd)) {
                                    walletToAddKeysTo.addKey(keyToAdd);

                                    // Update earliest transaction date.
                                    if (privateKeyAndDate.getDate() == null) {
                                        // Need to go back to the genesis block.
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

                    log.debug(walletToAddKeysTo.toString());

                    controller.getFileHandler().savePerWalletModelData(finalPerWalletModelData, false);
                    controller.getModel().createAddressBookReceivingAddresses(finalPerWalletModelData.getWalletFilename());
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            if (finalImportPanel != null) {
                                finalImportPanel.setMessageText(finalController.getLocaliser().getString(
                                        "importPrivateKeysSubmitAction.privateKeysImportSuccess"));
                            }
                        }
                    });

                    // Begin blockchain replay - returns quickly - just kicks it off.
                    log.debug("Starting replay from date = " + earliestTransactionDate);
                    if (performReplay) {
                        controller.getMultiBitService().replayBlockChain(earliestTransactionDate);
                        successMeasure = Boolean.TRUE;
                        statusBarMessage = controller.getLocaliser().getString("resetTransactionsSubmitAction.startReplay");
                    }
                } catch (WalletSaveException wse) {
                    log.error(wse.getClass().getName() + " " + wse.getMessage());
                    uiMessage = controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { wse.getMessage() });
                } catch (EncrypterDecrypterException ede) {
                    log.error(ede.getClass().getName() + " " + ede.getMessage());
                    uiMessage = controller.getLocaliser().getString("importPrivateKeysSubmitAction.privateKeysImportFailure",
                            new Object[] { ede.getMessage() });
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
                } finally {
                    if (reencryptionRequired) {
                        finalPerWalletModelData.getWallet().encrypt(walletPassword);
                    }
                }
                return successMeasure;
            }

            protected void done() {
                try {
                    Boolean wasSuccessful = get();

                    if (finalImportPanel != null && uiMessage != null) {
                        finalImportPanel.setMessageText(uiMessage);
                    }

                    MessageManager.INSTANCE.addMessage(new Message(statusBarMessage));

                    if (wasSuccessful) {
                        log.debug(statusBarMessage);
                        finalImportPanel.clearPasswords();
                    } else {
                        log.error(statusBarMessage);
                    }
                } catch (Exception e) {
                    // Not really used but caught so that SwingWorker shuts down cleanly.
                    log.error(e.getClass() + " " + e.getMessage());
                }
            }
        };
        log.debug("Importing private keys in background SwingWorker thread");
        worker.execute();
    }

    /**
     * This method is here because there is no equals on ECKey.
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

    // Used in testing.
    public void setPerformReplay(boolean performReplay) {
        this.performReplay = performReplay;
    }
}