package org.multibit.action;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

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
                    try {
                        File file = new File((String) (item.getNewValue()));
                        Wallet wallet = Wallet.loadFromFile(file);
                        controller.getModel().setWalletFilename(file.getAbsolutePath());
                        controller.getModel().setWallet(wallet);

                        // wire up the controller as a wallet event listener
                        final MultiBitController finalController = controller;
                        controller.getModel().addWalletEventListener(new WalletEventListener() {
                            public void onCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance,
                                    BigInteger newBalance) {
                                finalController.onCoinsReceived(wallet, transaction, prevBalance, newBalance);
                            }

                            public void onPendingCoinsReceived(Wallet wallet, Transaction transaction, BigInteger prevBalance,
                                    BigInteger newBalance) {
                                finalController.onPendingCoinsReceived(wallet, transaction, prevBalance, newBalance);
                            }
                        });

                    } catch (IOException ioe) {
                        controller
                                .displayMessage(
                                        "openWalletSubmitAction.walletNotLoaded",
                                        new Object[] { (String) item.getNewValue(),
                                                ioe.getClass().getName() + ": " + ioe.getMessage() },
                                        "openWalletSubmitAction.walletNotLoadedMessageBoxTitle");
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
