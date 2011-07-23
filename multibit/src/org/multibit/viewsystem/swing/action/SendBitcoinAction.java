package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFrame;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.SendBitcoinDialog;

/**
 * This {@link Action} sends bitcoin
 */
public class SendBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private JFrame mainFrame;
    private MultiBitController controller;
    private Localiser localiser;
    
    /**
     * Creates a new {@link SendBitcoinAction}.
     */
    public SendBitcoinAction(MultiBitController controller, Localiser localiser, ImageIcon icon, JFrame mainFrame) {
        super(localiser.getString("sendBitcoinAction.text"), icon);
        this.controller = controller;
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("sendBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("sendBitcoinAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * send bitcoins
     */
    public void actionPerformed(ActionEvent e) {        
        SendBitcoinDialog sendBitcoinsDialog = new SendBitcoinDialog(mainFrame, controller, localiser);       
        sendBitcoinsDialog.setVisible(true);      
    }
}