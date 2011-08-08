package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.AddressBookData;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.ECKey;

/**
 * an action to create or edit an address
 * 
 * @author jim
 * 
 */
public class CreateOrEditAddressAction implements Action {

    private MultiBitController controller;

    private boolean isCreate;
    private boolean isReceiving;

    public CreateOrEditAddressAction(MultiBitController controller, boolean isCreate, boolean isReceiving) {
        this.controller = controller;
        this.isCreate = isCreate;
        this.isReceiving = isReceiving;
    }

    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();

            if (data != null) {
                // copy the address and label to the user preferences so the
                // next view gets populated correctly
                if (dataProvider != null) {
                    String receiveAddress = null;
                    String receiveLabel = null;

                    Item receiveAddressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                    if (receiveAddressItem != null && receiveAddressItem.getNewValue() != null) {
                        receiveAddress = (String) receiveAddressItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.RECEIVE_ADDRESS, receiveAddress);
                    }
                    Item receiveLabelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                    if (receiveLabelItem != null && receiveLabelItem.getNewValue() != null) {
                        receiveLabel = (String) receiveLabelItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.RECEIVE_LABEL, receiveLabel);
                    }

                    if (receiveAddress != null) {
                        if (receiveLabel == null) {
                            receiveLabel = "";
                        }
                        controller.getModel().getAddressBook()
                                .addReceivingAddress(new AddressBookData(receiveLabel, receiveAddress), false);
                    }
                }
                Item addressItem;
                Item labelItem;
                if (isReceiving) {
                    String address = null;
                    String label = null;
                    addressItem = data.getItem(MultiBitModel.RECEIVE_ADDRESS);
                    if (addressItem != null && addressItem.getNewValue() != null) {
                        address = (String) addressItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.RECEIVE_ADDRESS, address);
                    }

                    labelItem = data.getItem(MultiBitModel.RECEIVE_LABEL);
                    if (labelItem != null && labelItem.getNewValue() != null) {
                        label = (String) labelItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.RECEIVE_LABEL, label);
                    }

                    if (address != null) {
                        if (label == null) {
                            label = "";
                        }
                        controller.getModel().getAddressBook().addReceivingAddress(new AddressBookData(label, address), false);
                    }
                } else {
                    String address = null;
                    String label = null;

                    addressItem = data.getItem(MultiBitModel.SEND_ADDRESS);
                    if (addressItem != null && addressItem.getNewValue() != null) {
                        address = (String) addressItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.SEND_ADDRESS, address);
                    }

                    labelItem = data.getItem(MultiBitModel.SEND_LABEL);
                    if (labelItem != null && labelItem.getNewValue() != null) {
                        label = (String) labelItem.getNewValue();
                        controller.getModel().setUserPreference(MultiBitModel.SEND_LABEL, label);
                    }

                    if (address != null) {
                        if (label == null) {
                            label = "";
                        }
                        controller.getModel().getAddressBook().addSendingAddress(new AddressBookData(label, address));
                    }
                }
            }
        }

        if (isCreate) {
            if (isReceiving) {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_RECEIVING_ADDRESS);
            } else {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_CREATE_NEW_SENDING_ADDRESS);
            }
        } else {
            if (isReceiving) {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_EDIT_RECEIVING_ADDRESS);
            } else {
                controller.setActionForwardToChild(ActionForward.FORWARD_TO_EDIT_SENDING_ADDRESS);
            }
        }
    }

    public String getDisplayText() {
        // TODO localise
        if (isCreate) {
            return "createNew";

        } else {
            return "edit";
        }
    }
}
