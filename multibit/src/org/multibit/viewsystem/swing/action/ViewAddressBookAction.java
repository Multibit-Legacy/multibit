package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.AddressBookDialog;

/**
 * This {@link Action} views the address book
 */
public class ViewAddressBookAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitFrame mainFrame;
    private Localiser localiser;
    
    /**
     * Creates a new {@link ViewAddressBookAction}.
     */
    public ViewAddressBookAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("viewAddressBookAction.text"), icon);
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("viewAddressBookAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("viewAddressBookAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * view address book
     */
    public void actionPerformed(ActionEvent e) {        
        AddressBookDialog addressBookFrame = new AddressBookDialog(localiser, mainFrame);
        addressBookFrame.setVisible(true);
    }
}