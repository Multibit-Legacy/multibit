package org.multibit.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.Localiser;
import org.multibit.ReceiveBitcoinsDialog;
import org.multibit.MultiBitFrame;

/**
 * This {@link Action} sends bitcoin
 */
public class ReceiveBitcoinAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457765L;

    private MultiBitFrame mainFrame;
    private Localiser localiser;
    
    /**
     * Creates a new {@link ReceiveBitcoinAction}.
     */
    public ReceiveBitcoinAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("receiveBitcoinAction.text"), icon);
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("receiveBitcoinAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("receiveBitcoinAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * receive bitcoins
     */
    public void actionPerformed(ActionEvent e) {        
        ReceiveBitcoinsDialog receiveBitcoinsDialog = new ReceiveBitcoinsDialog(mainFrame, localiser);       
        receiveBitcoinsDialog.setVisible(true);      
    }
}