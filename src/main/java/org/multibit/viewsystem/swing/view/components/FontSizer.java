package org.multibit.viewsystem.swing.view.components;

import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;

import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class FontSizer {
    private static final double NORMAL_SCREEN_RESOLUTION = 72.0;

    private MultiBitController controller;

    public FontSizer(MultiBitController controller) {
        this.controller = controller;
    }

    public void setAdjustedFont(Component component, int unadjustedFontSize) {
        component.setFont(getAdjustedFont(unadjustedFontSize));
    }

    public void setAdjustedFont(TitledBorder border, int unadjustedFontSize) {
        border.setTitleFont(getAdjustedFont(unadjustedFontSize));
    }

    public Font getAdjustedFont(int unadjustedFontSize) {
        int screenResolution = Toolkit.getDefaultToolkit().getScreenResolution();
        int fontSize = (int) Math.round(unadjustedFontSize * screenResolution / NORMAL_SCREEN_RESOLUTION);

        String fontStyleString = controller.getModel().getUserPreference(MultiBitModel.FONT_STYLE);
        int fontStyle = MultiBitFrame.MULTIBIT_DEFAULT_FONT_STYLE;

        try {
            fontStyle = Integer.parseInt(fontStyleString);
        } catch (NumberFormatException nfe) {
            // use default
        }

        String fontName = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
        if (fontName == null || "".equals(fontName)) {
            fontName = MultiBitFrame.MULTIBIT_DEFAULT_FONT_NAME;
        }

        Font font = new Font(fontName, fontStyle, fontSize);
        return font;
    }
}
