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
                Item receiveAmountItem = data.getItem(MultiBitModel.RECEIVE_AMOUNT);
                Item receiveAddressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                Item receiveLabelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                               
                String receiveAmount = "";
                String receiveAddress = "";
                String receiveLabel = "";
                
                if (receiveAmountItem != null && receiveAmountItem.getNewValue() != null) {
                    receiveAmount = (String) receiveAmountItem.getNewValue();
                }
                
                if (receiveAddressItem != null && receiveAddressItem.getNewValue() != null) {
                    receiveAddress = (String) receiveAddressItem.getNewValue();
                }
                
                if (receiveLabelItem != null && receiveLabelItem.getNewValue() != null) {
                    receiveLabel = (String) receiveLabelItem.getNewValue();
                }
                
                String bitcoinURI = BitcoinURI.convertToBitcoinURI(receiveAddress, receiveAmount, receiveLabel);
                
                // copy to clipboard
                TextTransfer textTransfer = new TextTransfer();
                textTransfer.setClipboardContents(bitcoinURI);
            }
        }
    }
}
