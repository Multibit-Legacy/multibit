package org.multibit.viewsystem.swing.view;

import java.io.File;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

public class WalletFileFilter extends javax.swing.filechooser.FileFilter {

    private static final String MAC_APPLICATION_SUFFIX = ".app";
    private MultiBitController controller;
    
    public WalletFileFilter(MultiBitController controller) {
        this.controller = controller;
    }
    
    public boolean accept(File file) {
        return (file.isDirectory() && !(file.getName().toLowerCase().endsWith(MAC_APPLICATION_SUFFIX))) || (file.getName().toLowerCase().endsWith(MultiBitModel.WALLET_FILE_EXTENSION));
    }

    public String getDescription() {
        String multiBitText = controller.getLocaliser().getString("multiBitFrame.title");
        return multiBitText + " (*." + MultiBitModel.WALLET_FILE_EXTENSION + ")";
    }
}
