package org.multibit.action;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.network.MultiBitService;

import com.google.bitcoin.core.Transaction;
import com.google.bitcoin.core.Wallet;
import com.google.bitcoin.core.WalletEventListener;

/**
 * an action to process the submit of the Open Wallet view
 * 
 * @author jim
 * 
 */
public class OpenWalletSubmitAction implements Action {

    private MultiBitController controller;

    public OpenWalletSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get the file name from the data provider and see if it has changed
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                Item item = data.getItem(MultiBitModel.SELECTED_WALLET_FILENAME);
                if (item != null && item.getNewValue() != null && !item.getNewValue().equals(item.getOriginalValue())) {

                    String walletFilename = (String) (item.getNewValue());

                    // defensive check on file being a directory - should never happen
                    if (!(new File(walletFilename).isDirectory())) {
                        MultiBitService oldMultiBitService = controller.getMultiBitService();
                        oldMultiBitService.getPeerGroup().stop();
                        MultiBitService multiBitService = new MultiBitService(oldMultiBitService.isUseTestNet(),
                                walletFilename, controller);
                        controller.setMultiBitService(multiBitService);
                    }

                    controller.fireWalletChanged();
                    controller.setActionForwardToParent();
                }
            }
        } else {
            // should never happen return to parent view
            controller.setActionForwardToParent();
        }
    }

    public String getDisplayText() {
        // would not normally be seen
        return "openWalletSubmit";
    }
}
