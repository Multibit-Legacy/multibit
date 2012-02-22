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

import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

import org.multibit.viewsystem.swing.ColorAndFontConstants;

public class GradientPanel extends JPanel {

    private static final long serialVersionUID = 1992327373249499976L;

    protected void paintComponent(Graphics g)
    {
        Graphics2D g2d = (Graphics2D)g;
        Dimension d = this.getSize();

        g2d.setPaint(new GradientPaint(0, 0, ColorAndFontConstants.BACKGROUND_COLOR,
            0, d.height, ColorAndFontConstants.DARK_BACKGROUND_COLOR, true));
        g2d.fillRect(0, 0, d.width , d.height);
    }
}
