package org.multibit.viewsystem.swing.view.components;

import javax.swing.JLabel;

import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitLabel extends JLabel {

    private static final long serialVersionUID = -3434455262992702604L;

    public MultiBitLabel(String labelText, int alignment) {
        super(labelText, alignment);
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
    }

    public MultiBitLabel(String labelText) {
        super(labelText);
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
    }
}
