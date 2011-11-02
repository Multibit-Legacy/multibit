package org.multibit.action;

import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.qrcode.BitcoinURI;
import org.multibit.viewsystem.swing.action.TextTransfer;

/**
 * an action to copy the bitcoin URI in the supplied formbean to the system
 * clipboard
 * 
 * @author jim
 * 
 */
public class CopyQRCodeTextAction implements Action {

    public CopyQRCodeTextAction() {
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                boolean isReceive = false;

                Item isReceiveItem = data.getItem(MultiBitModel.IS_RECEIVE_BITCOIN);
                if (isReceiveItem != null && Boolean.TRUE.toString().equals(isReceiveItem.getNewValue())) {
                    isReceive = true;
                }
                Item amountItem = null;
                Item addressItem = null;
                Item labelItem = null;
                if (isReceive) {
                    amountItem = data.getItem(MultiBitModel.RECEIVE_AMOUNT);
                    addressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                    labelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                } else {
                    amountItem = data.getItem(MultiBitModel.SEND_AMOUNT);
                    addressItem = data.getItem(MultiBitModel.SEND_ADDRESS);
                    labelItem = data.getItem(MultiBitModel.SEND_LABEL);
                }
                String amount = "";
                String address = "";
                String label = "";

                if (amountItem != null && amountItem.getNewValue() != null) {
                    amount = (String) amountItem.getNewValue();
                }

                if (addressItem != null && addressItem.getNewValue() != null) {
                    address = (String) addressItem.getNewValue();
                }

                if (labelItem != null && labelItem.getNewValue() != null) {
                    label = (String) labelItem.getNewValue();
                }

                String bitcoinURI = BitcoinURI.convertToBitcoinURI(address, amount, label);

                // copy to clipboard
                TextTransfer textTransfer = new TextTransfer();
                textTransfer.setClipboardContents(bitcoinURI);
            }
        }
    }
}
