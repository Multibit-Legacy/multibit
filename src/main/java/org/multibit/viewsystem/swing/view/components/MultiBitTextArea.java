package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextArea;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitTextArea extends JTextArea {

    private static final long serialVersionUID = 3539740758937470378L;

    private MultiBitController controller;
    
    public MultiBitTextArea(String text, int height, int width, MultiBitController controller) {
        super(text, height, width);
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
