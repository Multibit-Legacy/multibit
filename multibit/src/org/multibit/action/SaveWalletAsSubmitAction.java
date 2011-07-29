package org.multibit.action;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.view.OpenWalletView;

import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Save Wallet As view
 * 
 * @author jim
 * 
 */
public class SaveWalletAsSubmitAction implements Action {

    private static final String SEPARATOR = "-";
    private static final String BACKUP_SUFFIX_FORMAT = "yyyyMMddHHmmss";
    
    private MultiBitController controller;

    public SaveWalletAsSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the file name from the data provider and see if it has changed
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item item = data.getItem(MultiBitModel.SELECTED_WALLET_FILENAME);
                if (item != null && item.getNewValue() != null) {

                    Wallet wallet = controller.getModel().getWallet();
                    if (wallet == null) {
                        controller.displayMessage("saveWalletAsSubmitAction.noWalletToSave",
                                "saveWalletAsSubmitAction.noWalletToSave");
                        return;
                    }

                    File file = new File((String) item.getNewValue());

                    try {
                        // create backup file if file exists
                        String backupFileName = null;
                        if (file.exists()) {
                            backupFileName = createBackupFilename(file.getAbsolutePath());

                            File backupFile = new File(backupFileName);
                            copyFile(file, backupFile);
                        }
                        wallet.saveToFile(file);
                        if (backupFileName == null) {
                            controller.displayMessage("saveWalletAsSubmitAction.walletSaved",
                                    new Object[] { file.getAbsolutePath() },
                                    "saveWalletAsSubmitAction.title");
                        } else {
                            controller.displayMessage(
                                    "saveWalletAsSubmitAction.walletSavedWithBackup", new Object[] {
                                            file.getAbsolutePath(), backupFileName },
                                    "saveWalletAsSubmitAction.title");
                        }
                    } catch (IOException e1) {
                        e1.printStackTrace();
                    }
                }
            }
            controller.setActionForwardToParent();          
        } else {
            // should never happen return to parent view
            controller.setActionForwardToParent();
        }
    }

    public String getDisplayText() {
        // would not normally be seen
        return "saveWalletAsSubmit";
    }
    
    private String createBackupFilename(String filename) {
        DateFormat dateFormat = new SimpleDateFormat(BACKUP_SUFFIX_FORMAT);
        String backupFilename = filename + SEPARATOR + dateFormat.format(new Date());
        
        return backupFilename;
    }

    private void copyFile(File sourceFile, File destinationFile) throws IOException {
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        FileInputStream fileInputStream = null;
        FileOutputStream fileOutpurStream = null;
        FileChannel source = null;
        FileChannel destination = null;
        try {
            fileInputStream = new FileInputStream(sourceFile);
            source = fileInputStream.getChannel();
            fileOutpurStream = new FileOutputStream(destinationFile);
            destination = fileOutpurStream.getChannel();
            long transfered = 0;
            long bytes = source.size();
            while (transfered < bytes) {
                transfered += destination.transferFrom(source, 0, source.size());
                destination.position(transfered);
            }
        } finally {
            if (source != null) {
                source.close();
            } else if (fileInputStream != null) {
                fileInputStream.close();
            }
            if (destination != null) {
                destination.close();
            } else if (fileOutpurStream != null) {
                fileOutpurStream.close();
            }
        }
    }
}
