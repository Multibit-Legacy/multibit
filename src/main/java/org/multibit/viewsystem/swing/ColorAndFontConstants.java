/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing;

import java.awt.Color;
import java.awt.Font;
import java.awt.SystemColor;

import javax.swing.UIManager;

/***
 * Class containing constants for the colors and fonts used as default in
 * MultiBit
 * 
 * @author jim
 * 
 */
public class ColorAndFontConstants {
    public static String MULTIBIT_DEFAULT_FONT_NAME;
    public static int MULTIBIT_DEFAULT_FONT_STYLE;
    public static int MULTIBIT_DEFAULT_FONT_SIZE;
    public static int MULTIBIT_LARGE_FONT_INCREASE = 1;

    public static Color BACKGROUND_COLOR = UIManager.get("Label.background") == null ? new Color(250, 250, 253) : (Color) UIManager
            .get("Label.background");
    public static Color DARK_BACKGROUND_COLOR;
    public static Color DEFAULT_VERY_LIGHT_BACKGROUND_COLOR = new Color(250, 250, 253);
    public static Color VERY_LIGHT_BACKGROUND_COLOR;

    public static Color SELECTION_FOREGROUND_COLOR = SystemColor.textHighlightText;
    public static Color SELECTION_BACKGROUND_COLOR = SystemColor.textHighlight;

    public static Color CREDIT_FOREGROUND_COLOR = Color.GREEN.darker().darker();
    public static Color DEBIT_FOREGROUND_COLOR = Color.RED.darker();
    public static Color SELECTION_CREDIT_FOREGROUND_COLOR = SystemColor.textHighlightText;
    public static Color SELECTION_DEBIT_FOREGROUND_COLOR = SystemColor.textHighlightText;

    public static Color DEFAULT_ALTERNATE_TABLE_COLOR = new Color(230, 230, 233);
    public static Color ALTERNATE_TABLE_COLOR;

    public static final int BRIGHTEN_CONSTANT = 3;
    
    public static void init() {
        MULTIBIT_DEFAULT_FONT_NAME = UIManager.get("Label.font") == null ? Font.DIALOG : ((Font) UIManager.get("Label.font"))
                .getFontName();
        MULTIBIT_DEFAULT_FONT_STYLE = UIManager.get("Label.font") == null ? 0 : ((Font) UIManager.get("Label.font")).getStyle();
        MULTIBIT_DEFAULT_FONT_SIZE = UIManager.get("Label.font") == null ? 13 : ((Font) UIManager.get("Label.font")).getSize() + 1;

        Color labelBackground = (Color)UIManager.get("Label.background");
        if (labelBackground != null) {
            // Brighten it.
            labelBackground = new Color(Math.min(255, labelBackground.getRed() + BRIGHTEN_CONSTANT), 
                    Math.min(255, labelBackground.getGreen() + BRIGHTEN_CONSTANT),
                    Math.min(255, labelBackground.getBlue() + BRIGHTEN_CONSTANT));
        }
        
        BACKGROUND_COLOR = labelBackground == null ? new Color(250, 250, 253) : labelBackground;
        DARK_BACKGROUND_COLOR = BACKGROUND_COLOR.darker();
        ALTERNATE_TABLE_COLOR = DEFAULT_ALTERNATE_TABLE_COLOR;
        VERY_LIGHT_BACKGROUND_COLOR = DEFAULT_VERY_LIGHT_BACKGROUND_COLOR;
        
        SELECTION_FOREGROUND_COLOR = SystemColor.textHighlightText;
        SELECTION_BACKGROUND_COLOR = SystemColor.textHighlight;
    }
}
