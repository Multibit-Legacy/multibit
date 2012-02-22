package org.multibit.viewsystem.swing;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JTable;

/***
 * Class containing constants for the colors and fonts used as default in MultiBit
 * @author jim
 *
 */
public class ColorAndFontConstants {
    public static final String MULTIBIT_DEFAULT_FONT_NAME = "Dialog";
    public static final int MULTIBIT_DEFAULT_FONT_STYLE = Font.PLAIN;
    public static final int MULTIBIT_DEFAULT_FONT_SIZE = 13;
    public static final int MULTIBIT_LARGE_FONT_INCREASE = 2;

    public static final Color GOLD_COLOR = new Color(212, 160, 23);

    public static final Color BACKGROUND_COLOR = new Color(244, 244, 246);
    public static final Color VERY_LIGHT_BACKGROUND_COLOR = new Color(254, 254, 255);
    public static final Color DARK_BACKGROUND_COLOR = new Color(188, 212, 230); // beau
    // blue

    private static JTable COLOR_TABLE = new JTable();
    public static Color SELECTION_FOREGROUND_COLOR = COLOR_TABLE.getSelectionForeground();
    public static Color SELECTION_BACKGROUND_COLOR = COLOR_TABLE.getSelectionBackground();

}
