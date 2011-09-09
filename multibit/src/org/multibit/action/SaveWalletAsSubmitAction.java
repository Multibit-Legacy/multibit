package org.multibit.action;

import java.io.File;
import java.io.IOException;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.model.WalletInfo;
import org.multibit.network.FileHandler;
import org.multibit.network.MultiBitService;

import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Save Wallet As view
 * 
 * @author jim
 * 
 */
public class SaveWalletAsSubmitAction implements Action {

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

                    String newWalletFilename = (String) item.getNewValue();
                    
                    // if the filename has no extension, put on the wallet extension
                    if (newWalletFilename.indexOf(".") == -1) {
                        // add wallet file extension
                        newWalletFilename = newWalletFilename + "." + MultiBitModel.WALLET_FILE_EXTENSION;
                    }
                    
                    File newWalletFile = new File(newWalletFilename);

                    FileHandler fileHandler = new FileHandler(controller);
                    try {
                        // create backup file if file exists
                        String backupFileName = null;
                        if (newWalletFile.exists()) {
                            backupFileName = fileHandler.createBackupFile(newWalletFile);
                        }
                        fileHandler.saveWalletToFile(wallet, newWalletFile);

                        // also copy the info file so that the address labels get
                        // kept
                        if (item.getOriginalValue() != null) {
                            String sourceInfoFile = WalletInfo.createWalletInfoFilename((String) (item.getOriginalValue()));
                            String destinationInfoFile = WalletInfo.createWalletInfoFilename(newWalletFilename);
                            fileHandler.copyFile(new File(sourceInfoFile), new File(destinationInfoFile));
                        }
                        if (backupFileName == null) {
                            controller.displayMessage("saveWalletAsSubmitAction.walletSaved",
                                    new Object[] { newWalletFile.getAbsolutePath() }, "saveWalletAsSubmitAction.title");
                        } else {
                            controller.displayMessage("saveWalletAsSubmitAction.walletSavedWithBackup", new Object[] {
                                    newWalletFile.getAbsolutePath(), backupFileName }, "saveWalletAsSubmitAction.title");
                        }

                        // start using the new file as the wallet
                        MultiBitService oldMultiBitService = controller.getMultiBitService();
                        oldMultiBitService.getPeerGroup().stop();
                        MultiBitService multiBitService = new MultiBitService(oldMultiBitService.isUseTestNet(),
                                newWalletFilename, controller);
                        controller.setMultiBitService(multiBitService);

                        controller.fireWalletChanged();
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

}
