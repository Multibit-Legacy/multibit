package org.multibit.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.MultiBitFrame;

/**
 * This {@link Action} display the Help About message box
 */
public class HelpAboutAction extends AbstractAction {

    private static final long serialVersionUID = 191352235465057705L;

    private static final String SPLASH_ICON_FILE = "/images/splash.jpg";
    
    private MultiBitFrame mainFrame;

    private Localiser localiser;

    private ImageIcon imageIcon;

    /**
     * Creates a new {@link HelpAboutAction}.
     */
    public HelpAboutAction(Localiser localiser, MultiBitFrame mainFrame) {
        super(localiser.getString("helpAboutAction.text"));
        this.localiser = localiser;
        this.mainFrame = mainFrame;

        putValue(SHORT_DESCRIPTION, localiser.getString("helpAboutAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("helpAboutAction.mnemonicKey"));

        // messageBox icon
        //Image image = mainFrame.getIconImage();
        imageIcon = createImageIcon(SPLASH_ICON_FILE);
    }

    /**
     * show help about message box
     */
    public void actionPerformed(ActionEvent e) {
        String versionNumber = localiser.getVersionNumber();
        JOptionPane.showMessageDialog(mainFrame, localiser.getString("helpAboutAction.messageText", new Object[]{versionNumber}),
                localiser.getString("helpAboutAction.messageBoxTitle"), JOptionPane.INFORMATION_MESSAGE,
                imageIcon);
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err
                    .println("org.multibit.ViewerFrame#createImageIcon: Could not find file: "
                            + path);
            return null;
        }
    }
}