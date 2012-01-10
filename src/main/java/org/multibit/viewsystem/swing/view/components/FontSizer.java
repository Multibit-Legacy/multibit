package org.multibit.viewsystem.swing.view.components;

import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;

import org.multibit.viewsystem.swing.MultiBitFrame;

public class FontSizer {
    private static final double NORMAL_SCREEN_RESOLUTION = 72.0;

    public static void setAdjustedFont(Component component, int unadjustedFontSize) {
        int screenResolution = Toolkit.getDefaultToolkit().getScreenResolution();
        int fontSize = (int) Math.round(unadjustedFontSize * screenResolution / NORMAL_SCREEN_RESOLUTION);

        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE, fontSize);
        component.setFont(font);
    }
    
    public static Font getAdjustedFont(int unadjustedFontSize) {
        int screenResolution = Toolkit.getDefaultToolkit().getScreenResolution();
        int fontSize = (int) Math.round(unadjustedFontSize * screenResolution / NORMAL_SCREEN_RESOLUTION);

        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE, fontSize);
        return font;
    }
}
