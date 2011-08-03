package org.multibit.viewsystem.swing.view;

import java.util.Collection;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The view of the address book
 */
public class AddressBookView implements View {

    private static final long serialVersionUID = 191435612343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private static AddressBookDialog addressBookDialog;

    private boolean isReceiving;

    /**
     * Creates a new {@link AddressBookView}.
     */
    public AddressBookView(MultiBitController controller, Localiser localiser,
            MultiBitFrame mainFrame, boolean isReceiving) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
        this.isReceiving = isReceiving;
    }

    public String getDescription() {
        return localiser.getString("addressBookFrame.title");
    }

    /**
     * show address book dialog, sending addresses
     */
    public void displayView() {
        if (addressBookDialog == null) {
            addressBookDialog = new AddressBookDialog(controller, mainFrame, isReceiving);
        }
        addressBookDialog.displayTab(isReceiving);
        addressBookDialog.setVisible(true);
        addressBookDialog.selectRows();
        addressBookDialog.getReceivingAddressesTable().invalidate();
        addressBookDialog.getReceivingAddressesTable().validate();
        addressBookDialog.getReceivingAddressesTable().repaint();
        addressBookDialog.getSendingAddressesTable().invalidate();
        addressBookDialog.getSendingAddressesTable().validate();
        addressBookDialog.getSendingAddressesTable().repaint();

        // the action listeners of the code in the dialog do all the action
        // forwarding so nothing to do here
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        if (ViewSystem.newViewIsParentOfPrevious == relationshipOfNewViewToPrevious) {
            if (addressBookDialog != null) {
                addressBookDialog.setVisible(false);
                addressBookDialog.dispose();
                addressBookDialog = null;
            }
        }
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }
}