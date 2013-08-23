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
package org.multibit.viewsystem.swing.view.components;

import java.awt.*;

import javax.swing.JPanel;

import org.multibit.viewsystem.swing.ColorAndFontConstants;

public class HorizontalGradientPanel extends JPanel {

    private static final long serialVersionUID = 1992327444249499976L;

    private static final double PROPORTION_TO_FILL = 0.618; // golden ratio
    private ComponentOrientation componentOrientation;

    private double proportionToFill;
    
    public HorizontalGradientPanel(ComponentOrientation componentOrientation, double proportionToFill) {
        this.proportionToFill = proportionToFill;
        this.componentOrientation = componentOrientation;
        setOpaque(false);
    }

    public HorizontalGradientPanel(ComponentOrientation componentOrientation) {
        this(componentOrientation, PROPORTION_TO_FILL);
    }
    
    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2d = (Graphics2D)g;
        Dimension d = this.getSize();

        Color color1 = ColorAndFontConstants.BACKGROUND_COLOR;
        Color color2 = ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR;

        if (ComponentOrientation.LEFT_TO_RIGHT == componentOrientation) {
            g2d.setPaint(new GradientPaint(0, 0,  color1,
                    ((int)(d.width * proportionToFill)), 0, color2, false));
        } else {
            g2d.setPaint(new GradientPaint(((int)(d.width * (1 - proportionToFill))), 0, color2,
                    d.width, 0,  color1, false));
        }

         g2d.fillRect(0, 0, d.width , d.height);

        super.paintComponent(g);
    }
}
