package org.multibit.action;

import java.io.File;

import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
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
        fileHandler.writeUserPreferences();

        // save the wallet, including the wallet info
        fileHandler.saveWalletToFile(controller.getModel().getWallet(), new File(controller.getModel().getWalletFilename()));

        // shut down the PeerGroup
        if (controller.getMultiBitService() != null && controller.getMultiBitService().getPeerGroup() != null) {
            controller.getMultiBitService().getPeerGroup().stop();
        }

        System.exit(0);
    }

    public String getDisplayText() {
        // TODO localise
        return "exit";
    }
}
