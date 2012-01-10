package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextArea;

import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitTextArea extends JTextArea {

    private static final long serialVersionUID = 3539740758937470378L;

    public MultiBitTextArea(String text, int height, int width) {
        super(text, height, width);
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
    }
}
