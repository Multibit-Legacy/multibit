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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;

/**
 * Class containing constants for the colors and fonts used as default in
 * MultiBit
 */
public final class ColorAndFontConstants {
  private static final Logger log = LoggerFactory.getLogger(ColorAndFontConstants.class);

  public static String MULTIBIT_DEFAULT_FONT_NAME;
  public static int MULTIBIT_DEFAULT_FONT_STYLE;
  public static int MULTIBIT_DEFAULT_FONT_SIZE;
  public static int MULTIBIT_LARGE_FONT_INCREASE = 1;

  public static Color VERY_LIGHT_BACKGROUND_COLOR = new Color(251, 251, 254);
  public static Color BACKGROUND_COLOR = UIManager.get("Label.background") == null ? VERY_LIGHT_BACKGROUND_COLOR : (Color) UIManager
          .get("Label.background");
  public static Color MID_BACKGROUND_COLOR;

  public static Color SELECTION_FOREGROUND_COLOR = SystemColor.textHighlightText;
  public static Color SELECTION_BACKGROUND_COLOR = SystemColor.textHighlight;

  public static Color CREDIT_FOREGROUND_COLOR = Color.GREEN.darker().darker();
  public static Color DEBIT_FOREGROUND_COLOR = Color.RED.darker();

  public static Color ALTERNATE_TABLE_COLOR = new Color(230, 230, 233);

  public static Color TEXT_COLOR = Color.BLACK;
  public static Color DATA_HAS_CHANGED_TEXT_COLOR = Color.RED;

  public static final int BRIGHTEN_CONSTANT = 4;

  private static boolean inverse = false;

  /**
   * Utility class should not have a public constructor
   */
  private ColorAndFontConstants() {
  }

  public static void init() {
    MULTIBIT_DEFAULT_FONT_NAME = UIManager.get("Label.font") == null ? Font.DIALOG : ((Font) UIManager.get("Label.font"))
            .getFontName();
    MULTIBIT_DEFAULT_FONT_STYLE = UIManager.get("Label.font") == null ? 0 : ((Font) UIManager.get("Label.font")).getStyle();
    MULTIBIT_DEFAULT_FONT_SIZE = UIManager.get("Label.font") == null ? 13 : ((Font) UIManager.get("Label.font")).getSize() + 1;

    // Work out if we are using an inverse color scheme

    Color labelBackground = (Color) UIManager.get("Label.background");
    if (labelBackground != null) {
      log.debug("labelBackground = " + labelBackground.getRed() + " " + labelBackground.getGreen() + " " + labelBackground.getBlue());
      inverse = (labelBackground.getRed() + labelBackground.getGreen() + labelBackground.getBlue() < 384);

      // Brighten it.
      labelBackground = new Color(Math.min(255, labelBackground.getRed() + BRIGHTEN_CONSTANT),
              Math.min(255, labelBackground.getGreen() + BRIGHTEN_CONSTANT),
              Math.min(255, labelBackground.getBlue() + BRIGHTEN_CONSTANT));
    }

    // Logged simply for interest - it might be useful to determine inverse on some machines in the future.
    Color labelForeground = (Color) UIManager.get("Label.foreground");
    if (labelForeground != null) {
      log.debug("labelForeground = " + labelForeground.getRed() + " " + labelForeground.getGreen() + " " + labelForeground.getBlue());
    }

    if (inverse) {
      // Inverse color scheme
      VERY_LIGHT_BACKGROUND_COLOR = new Color(4, 4, 1);
      TEXT_COLOR = Color.WHITE;
      ALTERNATE_TABLE_COLOR = new Color(25, 25, 21);
      CREDIT_FOREGROUND_COLOR = Color.GREEN.brighter();
      DEBIT_FOREGROUND_COLOR = Color.RED.brighter();
    } else {
      // Normal color scheme
      VERY_LIGHT_BACKGROUND_COLOR = new Color(251, 251, 254);
      TEXT_COLOR = Color.BLACK;
      ALTERNATE_TABLE_COLOR = new Color(230, 230, 233);
      CREDIT_FOREGROUND_COLOR = Color.GREEN.darker().darker();
      DEBIT_FOREGROUND_COLOR = Color.RED.darker();
    }
    BACKGROUND_COLOR = labelBackground == null ? VERY_LIGHT_BACKGROUND_COLOR : labelBackground;

    MID_BACKGROUND_COLOR = new Color((VERY_LIGHT_BACKGROUND_COLOR.getRed() + BACKGROUND_COLOR.getRed()) / 2,
            (VERY_LIGHT_BACKGROUND_COLOR.getGreen() + BACKGROUND_COLOR.getGreen()) / 2,
            (VERY_LIGHT_BACKGROUND_COLOR.getGreen() + BACKGROUND_COLOR.getGreen()) / 2);

    SELECTION_FOREGROUND_COLOR = SystemColor.textHighlightText;
    SELECTION_BACKGROUND_COLOR = SystemColor.textHighlight;
  }

  public static boolean isInverse() {
    return inverse;
  }
}
