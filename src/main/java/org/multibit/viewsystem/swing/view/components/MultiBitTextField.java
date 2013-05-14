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
import java.awt.Font;
import java.awt.FontMetrics;

import javax.swing.JTextField;

import org.multibit.controller.Controller;

public class MultiBitTextField extends JTextField {

    private static final long serialVersionUID = 8706099337840832271L;
   
    private static final int HEIGHT_DELTA = 4;
    private static final int WIDTH_DELTA = 4;

    public MultiBitTextField(String text, int width, Controller controller) {
        this(text, width, controller, HEIGHT_DELTA);
    }

    public MultiBitTextField(String text, int width, Controller controller, int heightDelta) {
        super(text, width);
        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        setFont(font);
        
        FontMetrics fontMetrics = getFontMetrics(font);
        
        int preferredHeight = fontMetrics.getHeight() + heightDelta
        ;
        int preferredWidth = fontMetrics.getMaxAdvance() * width + WIDTH_DELTA;
        
        setPreferredSize(new Dimension(preferredWidth, preferredHeight));
        setMinimumSize(new Dimension(preferredWidth, preferredHeight));
    }
}
