package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import com.google.bitcoin.core.Wallet;

import org.multibit.Localiser;
import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} opens a wallet from a file
 */
public class OpenWalletAction extends AbstractAction {

    private static final long serialVersionUID = 1913592460523457705L;

    /**
     * static so that open and save dialogs use the same directory
     */
    private static JFileChooser fileChooser;
    private MultiBitFrame mainFrame;
    
    private MultiBitController controller;
    private Localiser localiser;
    
    /**
     * Creates a new {@link OpenWalletAction}.
     */
    public OpenWalletAction(MultiBitController controller, Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("openWalletAction.text"), icon);
        this.controller = controller;
        this.localiser = localiser;
        putValue(SHORT_DESCRIPTION, localiser.getString("openWalletAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("openWalletAction.mnemonicKey"));
        
        this.mainFrame = mainFrame;
        
        if (fileChooser == null) {
            fileChooser = new JFileChooser();
            fileChooser.setLocale(localiser.getLocale());
            
            if (mainFrame.getWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(mainFrame.getWalletFilename()));
            }
        }
    }

    /**
     * open a wallet
     */
    public void actionPerformed(ActionEvent e) { 
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_OPEN_WALLET);
        
        int returnVal = fileChooser.showOpenDialog(mainFrame);
        
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
           
            try {
                Wallet wallet = Wallet.loadFromFile(file);
                mainFrame.setWallet(wallet, file.getAbsolutePath());
            } catch (IOException ioe) {
                JOptionPane.showMessageDialog(mainFrame, localiser.getString(
                        "viewerFrame.walletNotLoaded", new Object[] { file , ioe.getClass().getName() + ": " + ioe.getMessage() }), localiser
                        .getString("viewerFrame.walletNotLoadedMessageBoxTitle"),
                        JOptionPane.ERROR_MESSAGE, new ImageIcon(mainFrame.getIconImage()));

            }
       } 
       controller.setActionForwardToParent();               
    }
    
    public static JFileChooser getFileChooser () {
        return fileChooser;
    }    
    
    public static void setFileChooser (JFileChooser fileChooser) {
        OpenWalletAction.fileChooser = fileChooser;
    }
}