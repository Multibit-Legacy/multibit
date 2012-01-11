package org.multibit.viewsystem.swing.view.components;

import java.awt.Component;
import java.awt.Font;
import java.awt.Toolkit;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public enum FontSizer {
        INSTANCE;
        
    private static final double NORMAL_SCREEN_RESOLUTION = 72.0;

    private MultiBitController controller;
    private Font adjustedDefaultFont;
    
    public void initialise(MultiBitController controller) {
        this.controller = controller;
        adjustedDefaultFont = createAdjustedDefaultFont();
    }
    
//    public void setAdjustedFont(Component component, int unadjustedFontSize) {
//        component.setFont(getAdjustedFont(unadjustedFontSize));
//    }
//
//    public void setAdjustedFont(TitledBorder border, int unadjustedFontSize) {
//        border.setTitleFont(getAdjustedFont(unadjustedFontSize));
//    }


    private Font createAdjustedDefaultFont() {
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        int unadjustedFontSize =  MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE;
        
        if (fontSizeString != null && !"".equals(fontSizeString)) {
            try {
                unadjustedFontSize = Integer.parseInt(fontSizeString);
            } catch (NumberFormatException nfe) {
               // use default        
            }
        }

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
    
//    public Font getAdjustedFont(int unadjustedFontSize) {
//        
//        int screenResolution = Toolkit.getDefaultToolkit().getScreenResolution();
//        int fontSize = (int) Math.round(unadjustedFontSize * screenResolution / NORMAL_SCREEN_RESOLUTION);
//
//        String fontStyleString = controller.getModel().getUserPreference(MultiBitModel.FONT_STYLE);
//        int fontStyle = MultiBitFrame.MULTIBIT_DEFAULT_FONT_STYLE;
//
//        try {
//            fontStyle = Integer.parseInt(fontStyleString);
//        } catch (NumberFormatException nfe) {
//            // use default
//        }
//
//        String fontName = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
//        if (fontName == null || "".equals(fontName)) {
//            fontName = MultiBitFrame.MULTIBIT_DEFAULT_FONT_NAME;
//        }
//
//        Font font = new Font(fontName, fontStyle, fontSize);
//        return font;
//    }
    
    public Font getAdjustedDefaultFont() {
        return adjustedDefaultFont;
    }
    
    /**
     * Get the required scaled font using the currently specified font size plus a delta
     * @param delta Delta from default font, in point size
     */
    public Font getAdjustedDefaultFontWithDelta(int delta) {
        Font scaledFont = adjustedDefaultFont.deriveFont(adjustedDefaultFont.getStyle(), adjustedDefaultFont.getSize() + delta);
        return scaledFont;
    }
}
