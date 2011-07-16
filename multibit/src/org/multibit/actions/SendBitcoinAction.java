package org.multibit.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.SendBitcoinsDialog;
import org.multibit.MultiBitFrame;

/**
 * This {@link Action} sends bitcoin
 */
public class SendBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitFrame mainFrame;
    private Localiser localiser;
    
    /**
     * Creates a new {@link SendBitcoinAction}.
     */
    public SendBitcoinAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("sendBitcoinAction.text"), icon);
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("sendBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("sendBitcoinAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * send bitcoins
     */
    public void actionPerformed(ActionEvent e) {        
        SendBitcoinsDialog sendBitcoinsDialog = new SendBitcoinsDialog(mainFrame, localiser);       
        sendBitcoinsDialog.setVisible(true);      
    }
}