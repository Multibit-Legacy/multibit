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

import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import com.google.bitcoin.core.WalletVersionException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import com.google.bitcoin.core.WalletMajorVersion;
import org.multibit.utils.ImageLoader;
import org.multibit.utils.VersionComparator;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * This {@link Action} migrates wallets from serialised to protobuf formats
 */
public class MigrateWalletsAction extends AbstractAction {

    public Logger log = LoggerFactory.getLogger(MigrateWalletsAction.class.getName());

    private static final long serialVersionUID = 1913592460523457705L;

    private MultiBitController controller;

    private MultiBitFrame mainFrame;

    private JOptionPane optionPane;

    /**
     * Creates a new {@link MigrateWalletAction}.
     */
    public MigrateWalletsAction(MultiBitController controller, MultiBitFrame mainFrame) {
        super(controller.getLocaliser().getString("migrateWalletsAction.text"));
        this.controller = controller;
        this.mainFrame = mainFrame;
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());

        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("migrateWalletsAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("migrateWalletsAction.mnemonicKey"));
    }

    /**
     * Ask the user if they want to migrate wallets now, then perform migration in background thread.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            List<String> walletFilenamesToMigrate = getWalletFilenamesToMigrate();
            
            if (walletFilenamesToMigrate.size() == 0) {
                // Nothing to do.
                return;
            }
            
            MultiBitDialog infoDialog = createDialog(mainFrame, walletFilenamesToMigrate.size());
            
            infoDialog.setVisible(true);
            
            Object value = optionPane.getValue();            
            if ((new Integer(JOptionPane.CANCEL_OPTION)).equals(value)) {
                controller.displayHelpContext(HelpContentsPanel.HELP_WALLET_FORMATS_URL);
                return;
            } else if ((new Integer(JOptionPane.CLOSED_OPTION)).equals(value)) {
                return;
            } else {
                boolean thereWereFailures = false;
                
                controller.displayView(View.MESSAGES_VIEW);
                MessageManager.INSTANCE.addMessage(new Message(" "));
                MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.start") + " " + controller.getLocaliser().getString("migrateWalletsAction.text") + "."));
                       
                FileHandler fileHandler = new FileHandler(controller);
                
                for (String walletFilename : walletFilenamesToMigrate) {
                    PerWalletModelData loopPerWalletModelData = controller.getModel().getPerWalletModelDataByWalletFilename(walletFilename);
                    
                    if (loopPerWalletModelData != null) {
                        MessageManager.INSTANCE.addMessage(new Message(" "));
                        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.isSerialisedAndNeedsMigrating", 
                                new Object[] {loopPerWalletModelData.getWalletDescription()})));

                        File tempDirectory = null;
                        File testWalletFile = null;
                        String walletMigrationErrorText = "Unknown";
                        try {
                            tempDirectory = createTempDirectory(new File(loopPerWalletModelData.getWalletFilename()));
                            testWalletFile = File.createTempFile("migrate", ".wallet", tempDirectory);
                            testWalletFile.deleteOnExit();
                          
                            // Copy the serialised wallet to the new test wallet location.
                            FileHandler.copyFile(new File(loopPerWalletModelData.getWalletFilename()), testWalletFile);

                            // Load serialised, change to protobuf, save, reload, check.
                            walletMigrationErrorText = convertToProtobufAndCheck(testWalletFile, fileHandler);
                            
                            // Did not save/ load as protobuf.
                            if (walletMigrationErrorText != null) {
                                thereWereFailures = true;
                                MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.testMigrationUnsuccessful")));
                                MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[]{walletMigrationErrorText})));
                                
                                // Save the MultiBit version so that we do not keep trying to run the migrate utility (until the next version).
                                loopPerWalletModelData.getWalletInfo().put(MultiBitModel.LAST_FAILED_MIGRATE_VERSION, controller.getLocaliser().getVersionNumber());
                                loopPerWalletModelData.setDirty(true);
                            }
                        } catch (IOException e1) {
                            migrationWasNotSuccessful(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (WalletLoadException e1) {
                            migrationWasNotSuccessful(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (WalletSaveException e1) {
                            migrationWasNotSuccessful(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (WalletVersionException wve) {
                            migrationWasNotSuccessful(loopPerWalletModelData, wve);
                            thereWereFailures = true;
                        } catch (IllegalStateException e1) {
                            migrationWasNotSuccessful(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (Exception e1) {
                            migrationWasNotSuccessful(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } finally {
                            // Delete test wallet data.
                            if (testWalletFile != null) {
                                PerWalletModelData testPerWalletModelData = controller.getModel().getPerWalletModelDataByWalletFilename(testWalletFile.getAbsolutePath());
                                controller.getModel().remove(testPerWalletModelData);
                                fileHandler.deleteWalletAndWalletInfo(testPerWalletModelData);
                                tempDirectory.delete();
                            }
                        }

                        try {
                            if (walletMigrationErrorText == null) {
                                // Test migrate was successful - now do it for real.
                                // Backup serialised wallet, specifying that it should not be migrated for this version of MultiBit.
                                // (It will likely ony be opened if the real migrate fails).
                                fileHandler.backupPerWalletModelData(loopPerWalletModelData, controller.getLocaliser().getVersionNumber());
                               MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.backingUpFile", 
                                        new Object[] {loopPerWalletModelData.getWalletBackupFilename()})));
                                           
                                // Load serialised, change to protobuf, save, reload, check.
                                walletMigrationErrorText = convertToProtobufAndCheck(new File(loopPerWalletModelData.getWalletFilename()), fileHandler);
                                
                                if (walletMigrationErrorText == null) {
                                    MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.success", 
                                            new Object[]{ loopPerWalletModelData.getWalletDescription()})));
                                    
                                    // Clear any 'lastMigrateFailed' properties.
                                    loopPerWalletModelData.getWalletInfo().remove(MultiBitModel.LAST_FAILED_MIGRATE_VERSION);
                                    loopPerWalletModelData.setDirty(true);
                                } else {                        
                                    MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.realMigrationUnsuccessful)")));
                                    MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[]{walletMigrationErrorText})));
                                    thereWereFailures = true;
                                }
                            }
                        } catch (WalletSaveException e1) {
                            migrationWasNotSuccessfulUseBackup(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (WalletVersionException e1) {
                            migrationWasNotSuccessfulUseBackup(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (WalletLoadException e1) {
                            migrationWasNotSuccessfulUseBackup(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (IllegalStateException e1) {
                            migrationWasNotSuccessfulUseBackup(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        } catch (Exception e1) {
                            migrationWasNotSuccessfulUseBackup(loopPerWalletModelData, e1);
                            thereWereFailures = true;
                        }
                    } 
                }
            
                MessageManager.INSTANCE.addMessage(new Message(" "));
                if (thereWereFailures) {
                    MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.mailJimErrors")));
                }
                MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.end") + " " + controller.getLocaliser().getString("migrateWalletsAction.text") + "."));
                MessageManager.INSTANCE.addMessage(new Message(" "));
                controller.fireDataChanged();
                controller.fireRecreateAllViews(false);
                controller.displayView(View.MESSAGES_VIEW);
            }                    
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }
    
    private void migrationWasNotSuccessful(PerWalletModelData perWalletModelData, Exception e) {
        // Save the MultiBit version so that we do not keep trying to run the migrate utility (until the next version).
        perWalletModelData.getWalletInfo().put(MultiBitModel.LAST_FAILED_MIGRATE_VERSION, controller.getLocaliser().getVersionNumber());
        perWalletModelData.setDirty(true);

        e.printStackTrace();
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.testMigrationUnsuccessful") + "\n" + localiseError(e)));
    }
    
    private void migrationWasNotSuccessfulUseBackup(PerWalletModelData perWalletModelData, Exception e) {
        perWalletModelData.getWalletInfo().put(MultiBitModel.LAST_FAILED_MIGRATE_VERSION, controller.getLocaliser().getVersionNumber());
        perWalletModelData.setDirty(true);

        e.printStackTrace();
        MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("migrateWalletsAction.realMigrationUnsuccessful") +  "\n" + localiseError(e)));
    }
    
    private String localiseError(Exception e) {
        String errorString = e.getClass().getCanonicalName() + " " + e.getMessage();
        if (e.getCause() != null) {
            errorString = errorString + ", (" + e.getCause().getClass().getCanonicalName() + " " + e.getCause().getMessage() + ")";
        }
        return controller.getLocaliser().getString("deleteWalletConfirmDialog.walletDeleteError2", new Object[]{e.getClass().getCanonicalName() + " " + e.getMessage() });
    }

    /**
     * @param walletFile
     * @param fileHandler
     * @return null if successful, localised error message describing problem if not
     */
    private String convertToProtobufAndCheck(File walletFile, FileHandler fileHandler) {
        // Load the newly copied test serialised file.
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);
        
        // Change wallet to protobuf.
        perWalletModelData.getWalletInfo().setWalletMajorVersion(WalletMajorVersion.PROTOBUF);
        perWalletModelData.getWallet().setMajorVersion(WalletMajorVersion.PROTOBUF);
        
        // Try to save it. This should save it in protobuf format
        fileHandler.savePerWalletModelData(perWalletModelData, true);
        
        // Load the newly saved test protobuf wallet.
        PerWalletModelData protobuf = fileHandler.loadFromFile(walletFile);
        
        if (protobuf == null) {
            return controller.getLocaliser().getString("migrateWalletsAction.theProtobufWalletWouldNotLoad");
        }
        
        // The new wallet should be protobuf.
        if (WalletMajorVersion.PROTOBUF != protobuf.getWalletInfo().getWalletMajorVersion()) {
            return controller.getLocaliser().getString("migrateWalletsAction.theWalletWasStillSerialised");
        }
        
        // Check the original and protobuf wallets are the same.
        return compareWallets(perWalletModelData, protobuf);
    }
    
    /**
     * Compare wallet data and return a null if no problems were encountered, or a localised error string if there were.
     * @param serialised
     * @param protobuf
     * @return String described error, or null if successful
     */
    private String compareWallets(PerWalletModelData serialised, PerWalletModelData protobuf) {       
        // Check the number of keys are the same.
        if (protobuf.getWallet().getKeychain().size() != serialised.getWallet().getKeychain().size()) {
            return controller.getLocaliser().getString("migrateWalletsAction.numberOfPrivateKeysAreDifferent", 
                    new Object[] {serialised.getWallet().getKeychain().size(), protobuf.getWallet().getKeychain().size()});
        }
        
        Set<String> protobufPrivateKeysAsStrings = new HashSet<String>();    
        ArrayList<ECKey> protobufKeys = protobuf.getWallet().keychain;
        for (ECKey ecKey : protobufKeys) {
            if (ecKey != null) {
                protobufPrivateKeysAsStrings.add(ecKey.toStringWithPrivate());
            }
        }
        
        // Every serialised private key should be in the protobuf.
        // (We do not care if the order is different).
        ArrayList<ECKey> serialisedKeys = serialised.getWallet().keychain;
        for (ECKey ecKey : serialisedKeys) {
            if (ecKey != null) {
                if (!protobufPrivateKeysAsStrings.contains(ecKey.toStringWithPrivate())) {
                    return controller.getLocaliser().getString("migrateWalletsAction.privateKeyWasMissing", 
                            new Object[] {ecKey.toAddress(controller.getModel().getNetworkParameters())});
                }
            }
        }
       
        // The balances should match.
        if (!serialised.getWallet().getBalance().equals(protobuf.getWallet().getBalance())) {
            return controller.getLocaliser().getString("migrateWalletsAction.balancesWereDifferent", 
                    new Object[]{serialised.getWallet().getBalance(), protobuf.getWallet().getBalance()});
        }
        if (!serialised.getWallet().getBalance(BalanceType.AVAILABLE).equals(protobuf.getWallet().getBalance(BalanceType.AVAILABLE))) {
            return controller.getLocaliser().getString("migrateWalletsAction.availableToSpendWereDifferent", 
                    new Object[]{serialised.getWallet().getBalance(BalanceType.AVAILABLE), protobuf.getWallet().getBalance(BalanceType.AVAILABLE)});
        }
        
        // Check the number of transactions are the same.
        Set<Transaction> protobufTransactions = protobuf.getWallet().getTransactions(true, true);
        Set<Transaction> serialisedTransactions = serialised.getWallet().getTransactions(true, true);
        if (protobufTransactions.size() != serialisedTransactions.size()) {
            return controller.getLocaliser().getString("migrateWalletsAction.numberOfTransactionsAreDifferent", 
                    new Object[] {serialisedTransactions.size(), protobufTransactions.size()});
        }
        
        // Check every transaction id in the serialised is in the protobuf.
        Set<String> protobufTransactionIds = new HashSet<String>();    
        for (Transaction protobufTx : protobufTransactions) {
            if (protobufTx != null) {
                protobufTransactionIds.add(protobufTx.getHashAsString());
            }
        }
        
        // Every serialised transaction should be in the protobuf.
        // (We do not care if the order is different).
       for (Transaction serialisedTx : serialisedTransactions) {
           if (serialisedTx != null) {
               if (!protobufTransactionIds.contains(serialisedTx.getHashAsString())) {
                   return controller.getLocaliser().getString("migrateWalletsAction.transactionWasMissing", new Object[] {serialisedTx.getHashAsString()});
               }
           }
        }
        
        // The two wallets are equal.
        return null;       
    }
    
    private List<String> getWalletFilenamesToMigrate() {
        List<String> walletFilenamesToMigrate = new ArrayList<String>();
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
            // Is it a serialized wallet ?
            if (loopPerWalletModelData.getWalletInfo() != null) {
                if (WalletMajorVersion.SERIALIZED == loopPerWalletModelData.getWalletInfo().getWalletMajorVersion()) {
                    // Have we already tried to migrate it with this version of MultiBit and failed ?
                    String lastMigrateVersion = loopPerWalletModelData.getWalletInfo().getProperty(MultiBitModel.LAST_FAILED_MIGRATE_VERSION);
                    VersionComparator versionComparator = new VersionComparator();
                    if (lastMigrateVersion == null || versionComparator.compare(controller.getLocaliser().getVersionNumber(), lastMigrateVersion) > 0) {
                        walletFilenamesToMigrate.add(loopPerWalletModelData.getWalletFilename());
                    }
                }
            }
        }
        return walletFilenamesToMigrate;
    }
    
    private MultiBitDialog createDialog(MultiBitFrame mainFrame, int numberOfWalletsToMigrate) {
        MultiBitDialog dialog = new MultiBitDialog(mainFrame, controller.getLocaliser().getString("migrateWalletsAction.text"));

        JButton okButton = new JButton(controller.getLocaliser().getString("okBackToParentAction.text"));
        okButton.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent arg0) {
                Object source = arg0.getSource();
                
                Object optionPaneObject = ((JButton)source).getParent().getParent();
                if (optionPaneObject instanceof JOptionPane) {
                    JOptionPane optionPane = (JOptionPane)optionPaneObject;
                    optionPane.setValue(JOptionPane.OK_OPTION);
                }                
                Object dialogObject = ((JButton)source).getTopLevelAncestor();
                 if (dialogObject instanceof JDialog) {
                    JDialog dialog = (JDialog)dialogObject;
                    dialog.setVisible(false);
                }
            }});
        JButton cancelButton = new JButton(controller.getLocaliser().getString("migrateWalletsAction.cancelAndShowHelp"));
        cancelButton.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent arg0) {
                Object source = arg0.getSource();
                
                Object optionPaneObject = ((JButton)source).getParent().getParent();
                if (optionPaneObject instanceof JOptionPane) {
                    JOptionPane optionPane = (JOptionPane)optionPaneObject;
                    optionPane.setValue(JOptionPane.CANCEL_OPTION);
                }                
                Object dialogObject = ((JButton)source).getTopLevelAncestor();
                if (dialogObject instanceof JDialog) {
                    JDialog dialog = (JDialog)dialogObject;
                    dialog.setVisible(false);
                }
        }});
        
        // Create an array of the text and components to be displayed.
        String information1 = controller.getLocaliser().getString("migrateWalletsAction.information1", new Object[]{numberOfWalletsToMigrate});
        String information2 = controller.getLocaliser().getString("migrateWalletsAction.information2");
        String information3 = " ";
        String information4 = controller.getLocaliser().getString("migrateWalletsAction.information3");
        Object[] array = {information1, information2, information3, information4};
 
        //Create an array specifying the number of dialog buttons and their text.
        Object[] options = {okButton, cancelButton};
 
        // Create the JOptionPane.
        optionPane = new JOptionPane(array,
                                    JOptionPane.QUESTION_MESSAGE,
                                    JOptionPane.YES_NO_OPTION,
                                    ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE),
                                    options,
                                    options[0]);
 
        // Make this dialog display it.
        dialog.setContentPane(optionPane);
        
        dialog.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        FontMetrics fontMetrics = dialog.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        int minimumHeight = fontMetrics.getHeight() * 8 + 40 ;
        int minimumWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("migrateWalletsAction.cancelAndShowHelp")) * 2, 
                fontMetrics.stringWidth(information1)) + 120;
        dialog.setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        dialog.positionDialogRelativeToParent(dialog, 0.5D, 0.47D);

 
        //Handle window closing correctly.
        dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        dialog.addWindowListener(new WindowAdapter() {
            @Override
                public void windowClosing(WindowEvent we) {
                /*
                 * Instead of directly closing the window,
                 * we're going to change the JOptionPane's
                 * value property.
                 */
                 optionPane.setValue(new Integer(
                                        JOptionPane.CLOSED_OPTION));
                 optionPane.getTopLevelAncestor().setVisible(false);
            }
        });
        
        return dialog;
    }
    
    /**
     * Create a temporary a temporary subdirectory whereever the baseFile is.
     * @param baseFile
     * @return
     * @throws IOException 
     */
    private File createTempDirectory(File baseFile) throws IOException {
        String currentDirectoryString = baseFile.getParent();
        
        String temporaryDirectoryString = currentDirectoryString + File.separator + "temp" + System.nanoTime();
        File tempDirectory = new File(temporaryDirectoryString);
        boolean dirWasMade = tempDirectory.mkdir();

        if (!dirWasMade) {
            throw new IllegalStateException("Could not create temporary directory");
        }
        
        tempDirectory.deleteOnExit();
            
        return tempDirectory;
    }
}