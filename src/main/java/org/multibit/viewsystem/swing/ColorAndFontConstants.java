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
 * Class containing constants for the colors and fonts used as default in MultiBit
 * @author jim
 *
 */
public class ColorAndFontConstants {
    public static final String MULTIBIT_DEFAULT_FONT_NAME =  ((Font)UIManager.get("Label.font")).getFontName();
    public static final int MULTIBIT_DEFAULT_FONT_STYLE = ((Font)UIManager.get("Label.font")).getStyle();
    public static final int MULTIBIT_DEFAULT_FONT_SIZE = ((Font)UIManager.get("Label.font")).getSize() + 1;
    public static final int MULTIBIT_LARGE_FONT_INCREASE = 2;

    public static final Color BACKGROUND_COLOR =  (Color) UIManager.get("Label.background");
    public static final Color VERY_LIGHT_BACKGROUND_COLOR = new Color(251, 251, 254);
    public static final Color DARK_BACKGROUND_COLOR = BACKGROUND_COLOR.darker();

    public static Color SELECTION_FOREGROUND_COLOR = SystemColor.textHighlightText;
    public static Color SELECTION_BACKGROUND_COLOR = SystemColor.textHighlight;
}
