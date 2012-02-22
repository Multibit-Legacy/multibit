/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.viewsystem.swing.view.components;

import java.awt.Font;
import java.awt.Toolkit;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.ColorAndFontConstants;

public enum FontSizer {
        INSTANCE;
        
    private static final double NORMAL_SCREEN_RESOLUTION = 72.0;

    private MultiBitController controller;
    private Font adjustedDefaultFont;
    
    public void initialise(MultiBitController controller) {
        this.controller = controller;
        adjustedDefaultFont = createAdjustedDefaultFont();
    }

    private Font createAdjustedDefaultFont() {
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        int unadjustedFontSize =  ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        
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
        int fontStyle = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_STYLE;

        try {
            fontStyle = Integer.parseInt(fontStyleString);
        } catch (NumberFormatException nfe) {
            // use default
        }

        String fontName = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
        if (fontName == null || "".equals(fontName)) {
            fontName = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_NAME;
        }

        Font font = new Font(fontName, fontStyle, fontSize);
        return font;
    }
    
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