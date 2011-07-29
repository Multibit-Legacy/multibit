package org.multibit.viewsystem.swing.view;

import java.io.File;
import java.util.Collection;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.action.SaveWalletAsSubmitAction;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The view for saving a wallet to a new file
 */
public class SaveWalletAsView implements View, DataProvider {

    private static final long serialVersionUID = 191987612343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private String selectedSaveWalletAsFilename;

    private JFileChooser fileChooser;

    /**
     * Creates a new {@link SaveWalletAsView}.
     */
    public SaveWalletAsView(MultiBitController controller, Localiser localiser,
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
        if (controller.getModel().getWalletFilename() != null) {
            fileChooser.setCurrentDirectory(new File(controller.getModel().getWalletFilename()));
        }

        int returnVal = fileChooser.showSaveDialog(mainFrame);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            if (file != null) {
                selectedSaveWalletAsFilename = file.getAbsolutePath();
                
                // delegate to save wallet as submit action
                SaveWalletAsSubmitAction submitAction = new SaveWalletAsSubmitAction(controller);
                submitAction.execute(this);
            } else {
                selectedSaveWalletAsFilename = null;
            }
        } else {
            selectedSaveWalletAsFilename = null;
        }

        // forward back to parent view ( = home page)
        controller.setActionForwardToParent();
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        JOptionPane.showMessageDialog(mainFrame, localiser.getString(messageKey, messageData),
                localiser.getString(titleKey), JOptionPane.INFORMATION_MESSAGE, new ImageIcon(
                        mainFrame.getIconImage()));
    }

    public void navigateAwayFromView(int nextViewId) {
        if (fileChooser != null) {
            fileChooser.setVisible(false);
        }
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
    }

    public Data getData() {
        Data data = new Data();
        Item selectedWalletFilenameItem = new Item(MultiBitModel.SELECTED_WALLET_FILENAME);
        selectedWalletFilenameItem.setOriginalValue(controller.getModel().getWalletFilename());
        selectedWalletFilenameItem.setNewValue(selectedSaveWalletAsFilename);

        data.addItem(MultiBitModel.SELECTED_WALLET_FILENAME, selectedWalletFilenameItem);
        return data;
    }
}