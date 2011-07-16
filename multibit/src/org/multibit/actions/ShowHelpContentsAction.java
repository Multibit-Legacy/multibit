package org.multibit.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.MultiBitFrame;

/**
 * This {@link Action} shows the BitCoinJ wiki
 */
public class ShowHelpContentsAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitFrame mainFrame;
    private Localiser localiser;
    
    /**
     * Creates a new {@link ShowHelpContentsAction}.
     */
    public ShowHelpContentsAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("showHelpContentsAction.text"), icon);
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("showHelpContentsAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("showHelpContentsAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
    }

    /**
     * show the BitCoinJ wiki
     */
    public void actionPerformed(ActionEvent e) {        
        JOptionPane.showMessageDialog(mainFrame, "TODO - Show Help Contents");
        
    }
}