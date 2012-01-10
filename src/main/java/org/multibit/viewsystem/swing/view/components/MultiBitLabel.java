package org.multibit.viewsystem.swing.view.components;

import javax.swing.JLabel;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitLabel extends JLabel {

    private static final long serialVersionUID = -3434455262992702604L;

    MultiBitController controller;

    public MultiBitLabel(String labelText, int alignment, MultiBitController controller) {
        super(labelText, alignment);
        this.controller = controller;
        setAdjustedFont();
    }

    public MultiBitLabel(String labelText, MultiBitController controller) {
        super(labelText);
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
