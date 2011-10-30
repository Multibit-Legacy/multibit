package org.multibit.action;

import java.io.File;
import java.util.List;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.model.PerWalletModelData;
import org.multibit.network.FileHandler;

/**
 * exit the application
 * 
 * @author jim
 * 
 */
public class ExitAction implements Action {
    private MultiBitController controller;

    public ExitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // write the user properties
        FileHandler fileHandler = new FileHandler(controller);

        // save all the wallets and put their filenames in the user preferences
        if (controller.getModel().getPerWalletModelDataList() != null) {
            List<PerWalletModelData> perWalletModelDataList = controller.getModel().getPerWalletModelDataList();

            int numberOfWallets = perWalletModelDataList.size();
            controller.getModel().setUserPreference(MultiBitModel.NUMBER_OF_WALLETS, numberOfWallets + "");
            controller.getModel().setUserPreference(MultiBitModel.ACTIVE_WALLET_FILENAME,
                    controller.getModel().getActiveWalletFilename());
            if (numberOfWallets > 0) {
                for (int i = 1; i <= numberOfWallets; i++) {
                    PerWalletModelData perWalletModelData = perWalletModelDataList.get(i - 1);
                    if (perWalletModelData.getWalletFilename() != null) {
                        controller.getModel().setUserPreference(MultiBitModel.WALLET_FILENAME_PREFIX + i,
                                perWalletModelData.getWalletFilename());
                        controller.getModel().setUserPreference(MultiBitModel.WALLET_DESCRIPTION_PREFIX + i,
                                perWalletModelData.getWalletDescription());
                        // save the ith wallet, including the wallet info
                        fileHandler.savePerWalletModelData(perWalletModelData);
                    }
                }

            }

        }

        fileHandler.writeUserPreferences();

        // shut down the PeerGroup
        if (controller.getMultiBitService() != null && controller.getMultiBitService().getPeerGroup() != null) {
            controller.getMultiBitService().getPeerGroup().stop();
        }

        System.exit(0);
    }
}
