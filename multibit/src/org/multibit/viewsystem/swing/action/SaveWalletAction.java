package org.multibit.viewsystem.swing.action;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import com.google.bitcoin.core.Wallet;
import org.multibit.viewsystem.Localiser;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * This {@link Action} saves a wallet, backing up any existing file with the
 * same name
 */
public class SaveWalletAction extends AbstractAction {

    private static final long serialVersionUID = 191352210565057705L;

    private MultiBitFrame mainFrame;

    private Localiser localiser;

    private ImageIcon imageIcon;

    /**
     * Creates a new {@link SaveWalletAction}.
     */
    public SaveWalletAction(Localiser localiser, ImageIcon icon, MultiBitFrame mainFrame) {
        super(localiser.getString("saveWalletAction.text"), icon);
        this.localiser = localiser;
        this.mainFrame = mainFrame;

        putValue(SHORT_DESCRIPTION, localiser.getString("saveWalletAction.tooltip"));
        putValue(MNEMONIC_KEY, localiser.getMnemonic("saveWalletAction.mnemonicKey"));

        // messageBox icon
        Image image = mainFrame.getIconImage();
        imageIcon = new ImageIcon(image);

        if (OpenWalletAction.getFileChooser() == null) {
            JFileChooser.setDefaultLocale(localiser.getLocale());
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setLocale(localiser.getLocale());
            if (mainFrame.getWalletFilename() != null) {
                fileChooser.setCurrentDirectory(new File(mainFrame.getWalletFilename()));
            }
            OpenWalletAction.setFileChooser(fileChooser);
        }
    }

    /**
     * Save wallet to a new file. Note - old files are always backed up to avoid
     * bitcoin loss
     */
    public void actionPerformed(ActionEvent e) {
        Wallet wallet = mainFrame.getWallet();
        if (wallet == null) {
            JOptionPane.showMessageDialog(mainFrame, localiser.getString(
                    "saveWalletAction.noWalletToSave"), localiser
                    .getString("saveWalletAction.noWalletToSave"),
                    JOptionPane.INFORMATION_MESSAGE, imageIcon);
            return;
        }

        JFileChooser fileChooser = OpenWalletAction.getFileChooser();

        int returnVal = fileChooser.showSaveDialog(mainFrame);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();

            try {
                // create backup file if file exists
                String backupFileName = null;
                if (file.exists()) {
                    Date now = new Date();
                    backupFileName = file.getAbsolutePath() + "-" + now.getTime();

                    File backupFile = new File(backupFileName);
                    copyFile(file, backupFile);
                }
                    wallet.saveToFile(file);
                    if (backupFileName == null) {
                        JOptionPane.showMessageDialog(mainFrame, localiser.getString(
                                "saveWalletAction.walletSaved",
                                new Object[] { file.getAbsolutePath() }), localiser
                                .getString("saveWalletAction.messageBoxTitle"),
                                JOptionPane.INFORMATION_MESSAGE, imageIcon);
                    } else {
                        JOptionPane.showMessageDialog(mainFrame, localiser.getString(
                                "saveWalletAction.walletSavedWithBackup",
                                new Object[] { file.getAbsolutePath(), backupFileName }), localiser
                                .getString("saveWalletAction.messageBoxTitle"),
                                JOptionPane.INFORMATION_MESSAGE, imageIcon);
                    }
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        }
    }

    private void copyFile(File sourceFile, File destinationFile) throws IOException {
        if (!destinationFile.exists()) {
            destinationFile.createNewFile();
        }
        FileInputStream fileInputStream = null;
        FileOutputStream fileOutpurStream = null;
        FileChannel source = null;
        FileChannel destination = null;
        try {
            fileInputStream = new FileInputStream(sourceFile);
            source = fileInputStream.getChannel();
            fileOutpurStream = new FileOutputStream(destinationFile);
            destination = fileOutpurStream.getChannel();
            long transfered = 0;
            long bytes = source.size();
            while (transfered < bytes) {
                transfered += destination.transferFrom(source, 0, source.size());
                destination.position(transfered);
            }
        } finally {
            if (source != null) {
                source.close();
            } else if (fileInputStream != null) {
                fileInputStream.close();
            }
            if (destination != null) {
                destination.close();
            } else if (fileOutpurStream != null) {
                fileOutpurStream.close();
            }
        }
    }
}