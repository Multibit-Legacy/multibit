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
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

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
import org.multibit.viewsystem.swing.view.WalletFileFilter;
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

    private String selectedWalletFilename;

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
     * Migrate wallets in a background thread.
     */
    public void actionPerformed(ActionEvent e) {
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        setEnabled(false);

        try {
            MultiBitDialog infoDialog = this.createDialog(mainFrame);
            
            infoDialog.setVisible(true);
            
            Object value = optionPane.getValue();
            System.out.println("MigrateWalletsAction value = " + value.toString());
            
            controller.displayView(View.MESSAGES_VIEW);
            MessageManager.INSTANCE.addMessage(new Message(" "));
            MessageManager.INSTANCE.addMessage(new Message("Start: " + controller.getLocaliser().getString("migrateWalletsAction.text")));
                       
            List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();
            for (PerWalletModelData loopPerWalletModelData : perWalletModelDataList) {
                if (WalletVersion.PROTOBUF == loopPerWalletModelData.getWalletInfo().getWalletVersion()) {
                    MessageManager.INSTANCE.addMessage(new Message("Wallet '"+ loopPerWalletModelData.getWalletDescription() + "' is already protobuf. Nothing to do."));
                } else {
                    if (WalletVersion.SERIALIZED == loopPerWalletModelData.getWalletInfo().getWalletVersion()) {
                        MessageManager.INSTANCE.addMessage(new Message("Wallet '"+ loopPerWalletModelData.getWalletDescription() + "' is serialised - needs migrating."));
                    } else {
                        MessageManager.INSTANCE.addMessage(new Message("Wallet '"+ loopPerWalletModelData.getWalletDescription() + "' is something else - leaving." ));
                    }
                }
            }
            
            MessageManager.INSTANCE.addMessage(new Message("End: " + controller.getLocaliser().getString("migrateWalletsAction.text")));
            MessageManager.INSTANCE.addMessage(new Message(" "));
                       
        } finally {
            setEnabled(true);
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
    }

    /**
     * open a wallet in a background Swing worker thread
     * @param selectedWalletFilename Filename of wallet to open
     */
    private void openWalletInBackground(String selectedWalletFilename) {
        final String selectedWalletFilenameFinal = selectedWalletFilename;

        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            
            private String message = null;
            
            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    log.debug("Opening wallet '" + selectedWalletFilenameFinal + "' in background swing worker");

                    controller.addWalletFromFilename(selectedWalletFilenameFinal);
                    controller.getModel().setActiveWalletByFilename(selectedWalletFilenameFinal);

                    // save the user properties to disk
                    FileHandler.writeUserPreferences(controller);
                    log.debug("User preferences with new wallet written successfully");
 
                    message = controller.getLocaliser().getString("multiBit.openingWalletIsDone", new Object[]{selectedWalletFilenameFinal});
                    
                    return Boolean.TRUE;
                } catch (WalletLoadException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                } catch (IOException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }  catch (WalletSaveException e) {
                    message = controller.getLocaliser().getString("openWalletSubmitAction.walletNotLoaded", new Object[]{selectedWalletFilenameFinal, e.getMessage()});
                    return Boolean.FALSE;
                }
            }
            
            protected void done() {
                try {
                    Boolean wasSuccessful = get();
                    if (wasSuccessful) {
                        log.debug(message);
                        MessageManager.INSTANCE.addMessage(new Message(message));  
                        controller.fireRecreateAllViews(false);
                        controller.fireDataChanged();
                    } else {
                        log.error(message);
                        MessageManager.INSTANCE.addMessage(new Message(message));
                    }
                } catch (Exception e) {
                    // not really used but caught so that SwingWorker shuts down cleanly
                    log.error(e.getClass() + " " + e.getMessage());
                } finally {
                    setEnabled(true);
                    if (mainFrame != null) {
                        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    }
                }
            }
        };
        log.debug("Executing open of wallet '" + selectedWalletFilenameFinal + "' in background swing worker");
        worker.execute();
    }
    
    private MultiBitDialog createDialog(MultiBitFrame mainFrame) {
        MultiBitDialog dialog = new MultiBitDialog(mainFrame, "A Title");

        JButton okButton = new JButton("OK");
        okButton.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent arg0) {
                Object source = arg0.getSource();
                ((JButton)source).getParent().getParent().getParent().setVisible(false);
                
            }});
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener(){
            @Override
            public void actionPerformed(ActionEvent arg0) {
                Object source = arg0.getSource();
                ((JButton)source).getParent().getParent().getParent().setVisible(false);
                
            }}); 
        //Create an array of the text and components to be displayed.
        String msgString1 = "MultiBit wants to update X wallets from the 'serialised' to 'protobuf' format";
        String msgString2 = "OK to do it now ?";
        Object[] array = {msgString1, msgString2};
 
        //Create an array specifying the number of dialog buttons
        //and their text.
        Object[] options = {okButton, cancelButton};
 
        //Create the JOptionPane.
        optionPane = new JOptionPane(array,
                                    JOptionPane.QUESTION_MESSAGE,
                                    JOptionPane.YES_NO_OPTION,
                                    ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE),
                                    options,
                                    options[0]);
 
        //Make this dialog display it.
        dialog.setContentPane(optionPane);
        
        dialog.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        FontMetrics fontMetrics = dialog.getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont());
        
        int minimumHeight = fontMetrics.getHeight() * 8 + 40 ;
        int minimumWidth = Math.max(fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT), fontMetrics.stringWidth(controller.getLocaliser().getString("sendBitcoinConfirmView.message"))) + 60;
        dialog.setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        dialog.positionDialogRelativeToParent(dialog, 0.5D, 0.47D);

 
//        //Handle window closing correctly.
//        dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
//        dialog.addWindowListener(new WindowAdapter() {
//                public void windowClosing(WindowEvent we) {
//                /*
//                 * Instead of directly closing the window,
//                 * we're going to change the JOptionPane's
//                 * value property.
//                 */
//                    optionPane.setValue(new Integer(
//                                        JOptionPane.CLOSED_OPTION));
//            }
//        });
        
        return dialog;
    }
}