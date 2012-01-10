package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextField;

import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitTextField extends JTextField {

    private static final long serialVersionUID = 8706099337840832271L;

    public MultiBitTextField(String text, int width) {
        super(text, width);
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
    }
}
