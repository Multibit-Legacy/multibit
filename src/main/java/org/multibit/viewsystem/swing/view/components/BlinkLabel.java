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

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.ColorAndFontConstants;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class BlinkLabel extends MultiBitLabel {
    private static final long serialVersionUID = 1L;

    public static final int BLINKING_TIME = 2000; // in ms

    private Color originalBackgroundColor;
    
    private boolean blinkEnabled;

    private String previousBlinkText;

    public BlinkLabel(Controller controller, boolean isLarge) {
        super("");
        
        if (isLarge) {
            setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(3 * ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE));
        }
        blinkEnabled = false;
        previousBlinkText = "";
        setOpaque(false);
    }

    public void blink(String newLabelText) {
        if (blinkEnabled) {
            if (checkTextHasChanged(newLabelText)) {
                originalBackgroundColor = getBackground();
                setForeground(ColorAndFontConstants.SELECTION_FOREGROUND_COLOR);
                setBackground(ColorAndFontConstants.SELECTION_BACKGROUND_COLOR);
                setOpaque(true);
 
                this.invalidate();
                this.validate();
                this.repaint();

                Timer timer = new Timer(BLINKING_TIME, new TimerListener(this));
                timer.setInitialDelay(BLINKING_TIME);
                timer.setRepeats(false);
                timer.start();
            }
        }
    }

    private synchronized boolean checkTextHasChanged(String newLabelText) {
        boolean isTextDifferent = newLabelText != null && !newLabelText.equals(previousBlinkText);
        setText(newLabelText);
        previousBlinkText = newLabelText;
        return isTextDifferent;
    }

    private class TimerListener implements ActionListener {
        BlinkLabel blinkLabel;

        public TimerListener(BlinkLabel blinkLabel) {
            this.blinkLabel = blinkLabel;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            blinkLabel.setForeground(ColorAndFontConstants.TEXT_COLOR);
            blinkLabel.setBackground(originalBackgroundColor);
            blinkLabel.setOpaque(false);
            
            blinkLabel.invalidate();
            blinkLabel.validate();
            blinkLabel.repaint();
        }
    }

    public void setBlinkEnabled(boolean blinkEnabled) {
        this.blinkEnabled = blinkEnabled;
    }
}
