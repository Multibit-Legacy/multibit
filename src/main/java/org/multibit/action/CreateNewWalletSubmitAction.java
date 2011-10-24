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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.ECKey;
import com.google.bitcoin.core.Wallet;

/**
 * an action to process the submit of the Create new Wallet view
 * 
 * @author jim
 * 
 */
public class CreateNewWalletSubmitAction implements Action {

    private static final Logger log = LoggerFactory.getLogger(CreateNewWalletSubmitAction.class);

    private MultiBitController controller;

    public CreateNewWalletSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the file name from the data provider
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item item = data.getItem(MultiBitModel.ACTIVE_WALLET_FILENAME);
                if (item != null && item.getNewValue() != null) {

                    String newWalletFilename = (String) item.getNewValue();

                    if (new File(newWalletFilename).isDirectory()) {
                        return;
                    }

                    // if the filename has no extension, put on the wallet
                    // extension
                    if (!newWalletFilename.contains(".")) {
                        // add wallet file extension
                        newWalletFilename = newWalletFilename + "." + MultiBitModel.WALLET_FILE_EXTENSION;
                    }

                    File newWalletFile = new File(newWalletFilename);

                    FileHandler fileHandler = new FileHandler(controller);
                    try {
                        // create backup file if file exists
                        if (newWalletFile.exists()) {
                           fileHandler.createBackupFile(newWalletFile);
                        }
                        String walletInfoFilename = WalletInfo.createWalletInfoFilename(newWalletFilename);
                        File walletInfoFile = new File(walletInfoFilename);
                        if (walletInfoFile.exists()) {
                            fileHandler.createBackupFile(walletInfoFile);
                         }
                        // create a new wallet
                        Wallet newWallet = new Wallet(controller.getMultiBitService().getNetworkParameters());
                        ECKey newKey = new ECKey();
                        newWallet.keychain.add(newKey);

                        WalletInfo newWalletInfo = new WalletInfo(newWalletFilename);
                        fileHandler.saveWalletAndWalletInfoToFile(newWallet, newWalletFile, newWalletInfo);                        
 
                        // start using the new file as the wallet
                        controller.addWalletFromFilename(newWalletFile.getAbsolutePath());
                        controller.getModel().setActiveWalletByFilename(newWalletFilename);
                        
                        // set a default description
                        String defaultDescription = controller.getLocaliser().getString("createNewWalletSubmitAction.defaultDescription");
                        controller.getModel().setWalletDescriptionByFilename(newWalletFile.getAbsolutePath(), defaultDescription);
                        
                        controller.fireNewWalletCreated();
                    } catch (IOException e) {
                        log.error("IOException: {}", e.getMessage(), e);
                    }
                }
            }
            controller.setActionForwardToParent();
        } else {
            // should never happen return to parent view
            controller.setActionForwardToParent();
        }
    }
}
