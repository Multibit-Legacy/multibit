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
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.multibit.controller.MultiBitController;
import org.multibit.file.FileHandler;
import org.multibit.file.WalletLoadException;
import org.multibit.file.WalletSaveException;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.model.WalletVersion;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitDialog;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This {@link Action} migrates wallets from serialised to protobuf formats
 */
public class MigrateWalletsAction extends AbstractAction {

    public Logger log = LoggerFactory.getLogger(MultiBitFrame.class.getName());

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
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            List<String> walletFilenamesToMigrate = getWalletFilenamesToMigrate();
            
            if (walletFilenamesToMigrate.size() == 0) {
                // Nothing to do.
                return;
            }
            
            MultiBitDialog infoDialog = this.createDialog(mainFrame, walletFilenamesToMigrate.size());
            
            infoDialog.setVisible(true);
            
            Object value = optionPane.getValue();
            System.out.println("MigrateWalletsAction value = " + value.toString());
            
            if ((new Integer(JOptionPane.CANCEL_OPTION)).equals(value)) {
                controller.displayHelpContext(HelpContentsPanel.HELP_WALLET_FORMATS_URL);
                return;
            } else if ((new Integer(JOptionPane.CLOSED_OPTION)).equals(value)) {
                return;
            } else {
                boolean thereWereFailures = false;
                
                // The block download needs to be paused.
                // Put a modal dialog up, or tweak existing one, so that user can not do anything else to the wallets. Needs to be able to abort.
                controller.displayView(View.MESSAGES_VIEW);
                MessageManager.INSTANCE.addMessage(new Message(" "));
                MessageManager.INSTANCE.addMessage(new Message("Start: " + controller.getLocaliser().getString("migrateWalletsAction.text")));
                       
                FileHandler fileHandler = new FileHandler(controller);
                
                for (String walletFilename : walletFilenamesToMigrate) {
                    PerWalletModelData loopPerWalletModelData = controller.getModel().getPerWalletModelDataByWalletFilename(walletFilename);
                    
                    if (loopPerWalletModelData != null && WalletVersion.SERIALIZED == loopPerWalletModelData.getWalletInfo().getWalletVersion()) {
                        MessageManager.INSTANCE.addMessage(new Message(" "));
                        MessageManager.INSTANCE.addMessage(new Message("Wallet '"+ loopPerWalletModelData.getWalletDescription() + "' is serialised - needs migrating."));

                        boolean walletOkToMigrate = false;
                        PerWalletModelData testPerWalletModelData  = null;
                        try {
                            File testWallet = File.createTempFile("migrateWallet", ".wallet");
                            testWallet.deleteOnExit();

                            String testWalletFilename = testWallet.getAbsolutePath();
                            File testWalletFile = new File(testWalletFilename);
                            
                            // Copy the serialised wallet to the new test wallet location.
                            FileHandler.copyFile(new File(loopPerWalletModelData.getWalletFilename()), testWalletFile);

                            // Load serialised, change to protobuf, save, reload, check.
                            walletOkToMigrate = convertToProtobufAndCheck(testWalletFile, fileHandler);
                            
                            // Did not save/ load as protobuf.
                            if (!walletOkToMigrate) {
                                thereWereFailures = true;
                                MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as 'serialised'."));
                            }
                        } catch (IOException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as serialised. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (WalletLoadException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as serialised. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (WalletSaveException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as serialised. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (IllegalStateException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as serialised. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (Exception e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Test wallet migration was not successful. Leaving wallet as serialised. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } finally {
                            // Delete test wallet.
                            if (testPerWalletModelData != null) {
                                fileHandler.deleteWalletAndWalletInfo(testPerWalletModelData);
                            }
                        }

                        try {
                            if (walletOkToMigrate) {
                                // Backup serialised wallet.
                                fileHandler.backupPerWalletModelData(loopPerWalletModelData);
                                MessageManager.INSTANCE.addMessage(new Message("Backing up serialised wallet to '" + loopPerWalletModelData.getWalletBackupFilename() + "'"));
                                           
                                // Load serialised, change to protobuf, save, reload, check.
                                boolean walletMigratedOk = convertToProtobufAndCheck(new File(loopPerWalletModelData.getWalletFilename()), fileHandler);
                                
                                if (walletMigratedOk) {
                                    MessageManager.INSTANCE.addMessage(new Message("Migration of wallet '" + loopPerWalletModelData.getWalletDescription() + "' to protobuf was successful."));
                                } else {                        
                                    MessageManager.INSTANCE.addMessage(new Message("Real wallet migration was not successful. Please reuse the backup."));
                                    thereWereFailures = true;
                                }
                            }
                        } catch (WalletSaveException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Wallet migration was not successful. Please reuse the backup. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (WalletLoadException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Wallet migration was not successful. Please reuse the backup. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (IllegalStateException e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Wallet migration was not successful. Please reuse the backup. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        } catch (Exception e1) {
                            e1.printStackTrace();
                            MessageManager.INSTANCE.addMessage(new Message("Wallet migration was not successful. Please reuse the backup. \nThe error was '" + e1.getClass().getCanonicalName() + " " + e1.getMessage() + "'"));
                            thereWereFailures = true;
                        }
                    } 
                }
            
                MessageManager.INSTANCE.addMessage(new Message(" "));
                if (thereWereFailures) {
                    MessageManager.INSTANCE.addMessage(new Message("To help improve the wallet migration code, please copy any error messages above and mail them to jim@multibit.org . Thanks."));
                }
                MessageManager.INSTANCE.addMessage(new Message("End: " + controller.getLocaliser().getString("migrateWalletsAction.text")));
                MessageManager.INSTANCE.addMessage(new Message(" "));
                controller.fireRecreateAllViews(false);
                controller.displayView(View.MESSAGES_VIEW);
            }
                       
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    private boolean convertToProtobufAndCheck(File walletFile, FileHandler fileHandler) {
        // Load the newly copied test serialised file.
        PerWalletModelData perWalletModelData = fileHandler.loadFromFile(walletFile);
        
        // Change wallet to protobuf.
        perWalletModelData.getWalletInfo().setWalletVersion(WalletVersion.PROTOBUF);
        
        // Try to save it. This should save it in protobuf format
        fileHandler.savePerWalletModelData(perWalletModelData, true);
        
        // Load the newly saved test protobuf wallet.
        PerWalletModelData protobuf = fileHandler.loadFromFile(walletFile);
        
        if (protobuf == null) {
            return false;
        }
        // Check the original and test new wallets are the same
        if (WalletVersion.PROTOBUF != protobuf.getWalletInfo().getWalletVersion()) {
            return false;
        } 
        
        // Check the number of private keys are the same.
        if (protobuf.getWallet().getKeychain().size() != perWalletModelData.getWallet().getKeychain().size()) {
            return false;
        }
        
        // Check the number of transactions are the same.
        if (protobuf.getWallet().getTransactions(true, true).size() != perWalletModelData.getWallet().getTransactions(true, true).size()) {
            return false;
        }
        
        // The conversion was ok.
        return true;
    }
    
    private List<String> getWalletFilenamesToMigrate() {
        List<String> walletFilenamesToMigrate = new ArrayList<String>();
        List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
        for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
            if (WalletVersion.SERIALIZED == loopPerWalletModelData.getWalletInfo().getWalletVersion()) {
                walletFilenamesToMigrate.add(loopPerWalletModelData.getWalletFilename());
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
                System.out.println("MigrateWalletsAction#createDialog - optionPaneObject = " + optionPaneObject);
                if (optionPaneObject instanceof JOptionPane) {
                    JOptionPane optionPane = (JOptionPane)optionPaneObject;
                    optionPane.setValue(JOptionPane.OK_OPTION);
                }                
                Object dialogObject = ((JButton)source).getTopLevelAncestor();
                System.out.println("MigrateWalletsAction#createDialog - dialogObject = " + dialogObject);
                if (dialogObject instanceof JDialog) {
                    JDialog dialog = (JDialog)dialogObject;
                    dialog.setVisible(false);
                }
            }});
        JButton cancelButton = new JButton(controller.getLocaliser().getString("cancelBackToParentAction.text") + ", show help");
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
        String information1 = "MultiBit wants to update " + numberOfWalletsToMigrate + " wallet(s)";
        String information2 = "from the 'serialised' to 'protobuf' format.";
        String information3 = "Ok to do it now ?";
        Object[] array = {information1, information2, information3};
 
        //Create an array specifying the number of dialog buttons
        //and their text.
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
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.message"))) + 60;
        dialog.setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        dialog.positionDialogRelativeToParent(dialog, 0.5D, 0.47D);

 
        //Handle window closing correctly.
        dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        dialog.addWindowListener(new WindowAdapter() {
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
}