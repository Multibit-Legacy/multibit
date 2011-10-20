package org.multibit.viewsystem.swing.view;

import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.CreateNewWalletSubmitAction;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The view for creating a new wallet
 */
public class CreateNewWalletView implements View, DataProvider {

    private static final long serialVersionUID = 191987612343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private String newWalletAsFilename;

    private JFileChooser fileChooser;

    /**
     * Creates a new {@link CreateNewWalletView}.
     */
    public CreateNewWalletView(MultiBitController controller, Localiser localiser,
            MultiBitFrame mainFrame) {
        this.controller = controller;
        this.localiser = localiser;
        this.mainFrame = mainFrame;
    }

    public String getDescription() {
        return localiser.getString("openWalletView.title");
    }

    /**
     * show open wallet view
     */
    public void displayView() {
        JFileChooser.setDefaultLocale(localiser.getLocale());
        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setLocale(localiser.getLocale());
        if (controller.getModel().getActiveWalletFilename() != null) {
            fileChooser.setCurrentDirectory(new File(controller.getModel().getActiveWalletFilename()));
        }
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(new WalletFileFilter(controller));
        String defaultFileName = fileChooser.getCurrentDirectory().getAbsoluteFile()  + File.separator + controller.getLocaliser().getString("saveWalletAsView.untitled") + "." + MultiBitModel.WALLET_FILE_EXTENSION;
        fileChooser.setSelectedFile(new File(defaultFileName));

        int returnVal = fileChooser.showSaveDialog(mainFrame);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            if (file != null) {
                newWalletAsFilename = file.getAbsolutePath();
                
                // delegate to create new wallet action
                CreateNewWalletSubmitAction submitAction = new CreateNewWalletSubmitAction(controller);
                submitAction.execute(this);
            } else {
                newWalletAsFilename = null;
            }
        } else {
            newWalletAsFilename = null;
        }

        // forward back to parent view ( = home page)
        controller.setActionForwardToParent();
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        JOptionPane.showMessageDialog(mainFrame, localiser.getString(messageKey, messageData),
                localiser.getString(titleKey), JOptionPane.INFORMATION_MESSAGE, new ImageIcon(
                        mainFrame.getIconImage()));
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        if (fileChooser != null) {
            fileChooser.setVisible(false);
        }
    }

    public Data getData() {
        Data data = new Data();
        Item activeWalletFilenameItem = new Item(MultiBitModel.ACTIVE_WALLET_FILENAME);
        activeWalletFilenameItem.setOriginalValue(controller.getModel().getActiveWalletFilename());
        activeWalletFilenameItem.setNewValue(newWalletAsFilename);

        data.addItem(MultiBitModel.ACTIVE_WALLET_FILENAME, activeWalletFilenameItem);
        return data;
    }
}