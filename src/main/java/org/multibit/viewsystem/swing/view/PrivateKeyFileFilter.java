package org.multibit.viewsystem.swing.view;

import java.io.File;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

public class PrivateKeyFileFilter extends javax.swing.filechooser.FileFilter {

    private MultiBitController controller;
    
    public PrivateKeyFileFilter(MultiBitController controller) {
        this.controller = controller;
    }
    
    public boolean accept(File file) {
        return (file.isDirectory()) || (file.getName().toLowerCase().endsWith(MultiBitModel.PRIVATE_KEY_FILE_EXTENSION));
    }

    public String getDescription() {
        String multiBitText = controller.getLocaliser().getString("multiBitFrame.title");
        return multiBitText + " (*." + MultiBitModel.PRIVATE_KEY_FILE_EXTENSION + ")";
    }
}
