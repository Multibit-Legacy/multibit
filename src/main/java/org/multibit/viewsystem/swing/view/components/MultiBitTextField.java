package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitTextField extends JTextField {

    private static final long serialVersionUID = 8706099337840832271L;

    private MultiBitController controller;
    
    public MultiBitTextField(String text, int width, MultiBitController controller) {
        super(text, width);
        this.controller = controller;
        setAdjustedFont();
    }
    
    private void setAdjustedFont() {
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        FontSizer fontSizer = new FontSizer(controller);
        if (fontSizeString == null || "".equals(fontSizeString)) {
            fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE);
        } else {
            try {
                fontSizer.setAdjustedFont(this, Integer.parseInt(fontSizeString));
            } catch (NumberFormatException nfe) {
                fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE);                
            }
        }
    }
}
