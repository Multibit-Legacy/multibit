package org.multibit.viewsystem.swing.view;

import java.io.File;
import java.util.Collection;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.action.OpenWalletSubmitAction;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The view of open wallet screen
 */
public class OpenWalletView implements View, DataProvider {

    private static final long serialVersionUID = 191987612343457705L;

    private MultiBitFrame mainFrame;

    private MultiBitController controller;

    private Localiser localiser;

    private  JFileChooser fileChooser;
    
    private String selectedWalletFilename;
    

    /**
     * Creates a new {@link OpenWalletView}.
     */
    public OpenWalletView(MultiBitController controller, Localiser localiser,
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
        if (fileChooser == null) {
            fileChooser = new JFileChooser();
            fileChooser.setLocale(localiser.getLocale());

            if (controller.getModel().getWalletFilename() != null) {
                fileChooser
                        .setCurrentDirectory(new File(controller.getModel().getWalletFilename()));
            }
            fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            fileChooser.setFileFilter(new WalletFileFilter(controller));
        }

        int returnVal = fileChooser.showOpenDialog(mainFrame);
        
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            selectedWalletFilename = file.getAbsolutePath();
            //delegate to open wallet submit action
            OpenWalletSubmitAction submitAction = new OpenWalletSubmitAction(controller);
            submitAction.execute(this);
        } else {
            selectedWalletFilename = null;
            fileChooser = null;
            controller.setActionForwardToParent();
        }
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        JOptionPane.showMessageDialog(
                mainFrame,
                localiser.getString(messageKey, messageData),
                localiser.getString(titleKey),
                JOptionPane.INFORMATION_MESSAGE, new ImageIcon(mainFrame.getIconImage()));
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
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
        selectedWalletFilenameItem.setNewValue(selectedWalletFilename);
  
        data.addItem(MultiBitModel.SELECTED_WALLET_FILENAME, selectedWalletFilenameItem);
        return data;
    }
}