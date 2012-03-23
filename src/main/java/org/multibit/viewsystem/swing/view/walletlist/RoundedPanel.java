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
package org.multibit.viewsystem.swing.view.walletlist;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.SystemColor;
import java.util.Locale;

import javax.swing.JPanel;

import org.multibit.viewsystem.swing.ColorAndFontConstants;

/**
 * RoundedPanel code from:
 * 
 * http://www.codeproject.com/KB/java/rounded-jpanel.aspx
 * http://www.opensource.org/licenses/apache2.0.php
 * 
 * @author b4rc0ll0
 * 
 */
public class RoundedPanel extends JPanel {

    private static final long serialVersionUID = 6690320592192612325L;
    /** Stroke size. it is recommended to set it to 1 for better view */
    protected int strokeSize = 1;
    /** Color of shadow */
    protected Color shadowColor = Color.DARK_GRAY;
    /** Sets if it drops shadow */
    protected boolean shady = true;
    /** Sets if it has an High Quality view */
    protected boolean highQuality = true;
    /** Double values for Horizontal and Vertical radius of corner arcs */
    protected Dimension arcs = new Dimension(6, 6);
    /** Distance between shadow border and opaque panel border */
    protected int shadowGap = 2;
    /** The offset of shadow. */
    protected int shadowOffset = 2;
    /** The transparency value of shadow. ( 0 - 255) */
    protected int shadowAlpha = 80;

    boolean selected;

    public RoundedPanel(Locale locale) {
        super();
        applyComponentOrientation(ComponentOrientation.getOrientation(locale));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        int width = getWidth();
        int height = getHeight();
             
        int shadowGap = this.shadowGap;
        Color shadowColorA = new Color(shadowColor.getRed(), shadowColor.getGreen(), shadowColor.getBlue(), shadowAlpha);
        Graphics2D graphics = (Graphics2D) g;

        // Sets antialiasing if HQ.
        if (highQuality) {
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }

        // Draws shadow borders if any.
        if (shady) {
            graphics.setColor(shadowColorA);
            graphics.fillRoundRect(shadowOffset,// X position
                    shadowOffset,// Y position
                    width - strokeSize - shadowOffset, // width
                    height - strokeSize - shadowOffset, // height
                    arcs.width, arcs.height);// arc Dimension
        } else {
            shadowGap = 1;
        }

        // Draws the rounded opaque panel with borders.
        graphics.setColor(getBackground());
        graphics.fillRoundRect(0, 0, width - shadowGap, height - shadowGap, arcs.width, arcs.height);

        if (selected) {
            graphics.setColor(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
            graphics.setStroke(new BasicStroke(strokeSize + 1));
        } else {
            graphics.setColor(SystemColor.windowBorder);
            graphics.setStroke(new BasicStroke(strokeSize));

        }
        graphics.drawRoundRect(0, 0, width - shadowGap, height - shadowGap, arcs.width, arcs.height);

        if (!selected) {
            graphics.fillRect(4, height - 1, width - 8, 1);
        }

        // Sets strokes to default, is better.
        graphics.setStroke(new BasicStroke());
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }
}
