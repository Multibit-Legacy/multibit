package org.multibit.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.MultiBitFrame;

/**
 * This {@link Action} views the BitCoinJ preferences
 */
public class ViewPreferencesAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitFrame mainFrame;
    private Localiser localiser;

    /**
     * Creates a new {@link ViewPreferencesAction}.
     */
    public ViewPreferencesAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("viewPreferencesAction.text"), icon);
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("viewPreferencesAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("viewPreferencesAction.mnemonicKey"));

        this.mainFrame = mainFrame;
    }

    /**
     * views the BitCoinJ preferences
     */
    public void actionPerformed(ActionEvent e) {
        JOptionPane.showMessageDialog(mainFrame, "TODO - View Preferences");
    }
}