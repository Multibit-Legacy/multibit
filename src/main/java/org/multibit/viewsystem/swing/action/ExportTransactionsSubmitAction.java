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

import com.googlecode.jcsv.CSVStrategy;
import com.googlecode.jcsv.writer.CSVWriter;
import com.googlecode.jcsv.writer.internal.CSVWriterBuilder;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.file.WalletTableDataEntryConverter;
import org.multibit.file.WalletTableDataHeaderEntryConverter;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.bitcoin.WalletData;
import org.multibit.model.bitcoin.WalletTableData;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.CsvFileFilter;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * This {@link Action} exports transactions from a wallet.
 */
public class ExportTransactionsSubmitAction extends AbstractAction {

    private static final Logger log = LoggerFactory.getLogger(CreateWalletSubmitAction.class);

    private static final long serialVersionUID = 1923492460523457765L;

    private final Controller controller;
    private final BitcoinController bitcoinController;
    
    private MultiBitFrame mainFrame;

    private Font adjustedFont;

    /**
     * Creates a new {@link ExportTransactionsSubmitAction}.
     */
    public ExportTransactionsSubmitAction(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
        super(bitcoinController.getLocaliser().getString("exportTransactionsSubmitAction.text"), ImageLoader.createImageIcon(ImageLoader.TRANSACTIONS_EXPORT_ICON_FILE));
        
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        this.mainFrame = mainFrame;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(controller.getLocaliser().getString("exportTransactionsSubmitAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("exportTransactionsSubmitAction.text"));
    }

    /**
     * Ask the user for a filename and then export transactions to there.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (mainFrame != null) {
            mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }
        setEnabled(false);

        try {
            // Create a file save dialog.
            JFileChooser.setDefaultLocale(controller.getLocaliser().getLocale());
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setLocale(controller.getLocaliser().getLocale());
            fileChooser.setDialogTitle( controller.getLocaliser().getString("showExportPrivateKeysPanel.filename.text.2"));

            adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
            if (adjustedFont != null) {
                setFileChooserFont(new Container[] {fileChooser});
            }

            String walletFilename = bitcoinController.getModel().getActiveWalletFilename();

            fileChooser.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            if (walletFilename != null) {
                fileChooser.setCurrentDirectory(new File(walletFilename));
            }
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            fileChooser.setFileFilter(new CsvFileFilter(controller));
            
            String defaultFileName;
            if (walletFilename != null) {
                // Find stem of filename.
                int suffixSeparatorLong = walletFilename.lastIndexOf('.');
                String stem = walletFilename.substring(0, suffixSeparatorLong);  
                defaultFileName = stem + "." + BitcoinModel.CSV_FILE_EXTENSION;
            } else {
                defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile() + File.separator
                + controller.getLocaliser().getString("saveWalletAsView.untitled") + "." + BitcoinModel.CSV_FILE_EXTENSION;
            }
            fileChooser.setSelectedFile(new File(defaultFileName));

            fileChooser.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            int returnVal = fileChooser.showSaveDialog(mainFrame);

            String exportTransactionsFilename = null;
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = fileChooser.getSelectedFile();
                if (file != null) {
                    exportTransactionsFilename = file.getAbsolutePath();
                    exportTransactions(exportTransactionsFilename);
                }
            }
        } finally {
            setEnabled(true);
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
        }
    }

    void exportTransactions(String exportTransactionsFilename) {
        String message;
        if (new File(exportTransactionsFilename).isDirectory()) {
            message = controller.getLocaliser().getString("exportTransactionsSubmitAction.fileIsADirectory",
                    new Object[] { exportTransactionsFilename });
            log.debug(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
            return;
        }

        // If the filename has no extension, put on the wallet extension.
        if (!exportTransactionsFilename.contains(".")) {
             exportTransactionsFilename = exportTransactionsFilename + "." + BitcoinModel.CSV_FILE_EXTENSION;
        }

        File exportTransactionsFile = new File(exportTransactionsFilename);
        
        // Check on file overwrite.
        if (exportTransactionsFile.exists()) {
            String yesText = controller.getLocaliser().getString("showOpenUriView.yesText");
            String noText = controller.getLocaliser().getString("showOpenUriView.noText");
            String questionText = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwrite",
                    new Object[] { exportTransactionsFile.getName() });
            String questionTitle = controller.getLocaliser().getString("showExportPrivateKeysAction.thisFileExistsOverwriteTitle");
            int selection = JOptionPane.showOptionDialog(mainFrame, questionText, questionTitle, JOptionPane.YES_NO_OPTION,
                    JOptionPane.QUESTION_MESSAGE, ImageLoader.createImageIcon(ImageLoader.QUESTION_MARK_ICON_FILE), new String[] {
                            yesText, noText }, noText);
            if (selection != JOptionPane.YES_OPTION) {
                return;
            }
            
            // Delete the existing file.
            boolean deleteWasSuccessful = exportTransactionsFile.delete();
            if (!deleteWasSuccessful) {
                String message2 = controller.getLocaliser().getString("exportTransactionsSubmitAction.genericCouldNotDelete",
                        new Object[] { exportTransactionsFilename});
                log.error(message2);
                MessageManager.INSTANCE.addMessage(new Message(message2));
                return;
            }
        }
        
        // Now actually perform the export.
        // (This is separated out to make it easier to test.)
        exportTransactionsDoIt(bitcoinController.getModel().getActivePerWalletModelData(), exportTransactionsFilename);
    }
    
    private void setFileChooserFont(Component[] comp) {
        for (int x = 0; x < comp.length; x++) {
            if (comp[x] instanceof Container)
                setFileChooserFont(((Container) comp[x]).getComponents());
            try {
                comp[x].setFont(adjustedFont);
            } catch (Exception e) {
            }// do nothing
        }
    }
    
    public void exportTransactionsDoIt(WalletData walletData, String exportTransactionsFilename) {        
        List<WalletTableData> walletTableDataList = bitcoinController.getModel().createWalletTableData(bitcoinController, walletData);
        
        // Sort by date descending.
        Comparator<WalletTableData> comparator = new Comparator<WalletTableData>() {
            @Override
            public int compare(WalletTableData o1, WalletTableData o2) {
                if (o1 == null) {
                    if (o2 == null) {
                        return 0;
                    } else {
                        return 1;
                    }
                } else {
                    if (o2 == null) {
                        return -1;
                    }
                }
                Date d1 = o1.getDate();
                Date d2 = o2.getDate();
                if (d1 == null) {
                    // Object 1 has missing date.
                    return 1;
                }
                if (d2 == null) {
                    // Object 2 has missing date.
                    return -1;
                }
                long n1 = d1.getTime();
                long n2 = d2.getTime();
                if (n1 == 0) {
                    // Object 1 has missing date.
                    return 1;
                }
                if (n2 == 0) {
                    // Object 2 has missing date.
                    return -1;
                }
                if (n1 < n2) {
                    return -1;
                } else if (n1 > n2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        };
        Collections.sort(walletTableDataList, Collections.reverseOrder(comparator));
        
        //Writer out = null;
        OutputStreamWriter outputStreamWriter = null;
        try {
            //out = new FileWriter(exportTransactionsFilename);
            outputStreamWriter = new OutputStreamWriter ( new FileOutputStream(exportTransactionsFilename, true ), "UTF-8" ); 
            
            // Write the header row.
            WalletTableDataHeaderEntryConverter headerConverter = new WalletTableDataHeaderEntryConverter();
            headerConverter.setBitcoinController(bitcoinController);
            CSVWriter<WalletTableData> csvHeaderWriter = new CSVWriterBuilder<WalletTableData>(outputStreamWriter).strategy(CSVStrategy.UK_DEFAULT)
                    .entryConverter(headerConverter).build();
            
            csvHeaderWriter.write(new WalletTableData(null));
            
            // Write the body of the CSV file.
            WalletTableDataEntryConverter converter = new WalletTableDataEntryConverter();
            converter.setBitcoinController(bitcoinController);
            CSVWriter<WalletTableData> csvWriter = new CSVWriterBuilder<WalletTableData>(outputStreamWriter).strategy(CSVStrategy.UK_DEFAULT)
                    .entryConverter(converter).build();
            
            csvWriter.writeAll(walletTableDataList);
            String message = controller.getLocaliser().getString("exportTransactionsSubmitAction.success",
                    new Object[] { exportTransactionsFilename });
            MessageManager.INSTANCE.addMessage(new Message(message));
        } catch (NullPointerException e) {
            String message = controller.getLocaliser().getString("exportTransactionsSubmitAction.failure",
                    new Object[] { exportTransactionsFilename, e.getClass().getName() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
        } catch (IOException e) {
            String message = controller.getLocaliser().getString("exportTransactionsSubmitAction.failure",
                    new Object[] { exportTransactionsFilename, e.getMessage() });
            log.error(message);
            MessageManager.INSTANCE.addMessage(new Message(message));
        } finally {
            if (outputStreamWriter != null) {
                try {
                    outputStreamWriter.flush();
                    outputStreamWriter.close();
                } catch (IOException ioe) {
                    String message = controller.getLocaliser().getString("exportTransactionsSubmitAction.failure",
                            new Object[] { exportTransactionsFilename, ioe.getMessage() });
                    log.error(message);
                    MessageManager.INSTANCE.addMessage(new Message(message));
                }
            }
        }
    }
}