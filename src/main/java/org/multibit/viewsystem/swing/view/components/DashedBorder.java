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

import javax.swing.border.Border;
import java.awt.*;
import java.util.Locale;

public class DashedBorder implements Border {
    public static final int THICKNESS = 2; // TODO Consider if dashWidth should be used instead
    Color color;
    int dashWidth;
    int dashHeight;

    Locale locale;

    public DashedBorder(Locale locale) {
        this(Color.LIGHT_GRAY, 4, 4);
        this.locale = locale;
    }

    public DashedBorder(Color c, int width, int height) {
        if (width < 1) {
            throw new IllegalArgumentException("Invalid width: " + width);
        }
        if (height < 1) {
            throw new IllegalArgumentException("Invalid height: " + height);
        }
        color = c;
        dashWidth = width;
        dashHeight = height;
    }

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        Insets insets = getBorderInsets(c);
        g.setColor(color);
        int numHigh = Math.round(height / dashHeight);
        int startPoint;

        for (int i = 0; i <= numHigh; i += 2) {
            startPoint = x + dashHeight * i;
            if (ComponentOrientation.getOrientation(locale).isLeftToRight()) {
                g.fillRect(x + width - insets.right, startPoint, THICKNESS, dashHeight);
            } else {
                g.fillRect(x + insets.left, startPoint, THICKNESS, dashHeight);

            }
        }
    }

    @Override
    public Insets getBorderInsets(Component c) {
        return new Insets(THICKNESS, THICKNESS, THICKNESS, THICKNESS);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}