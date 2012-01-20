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

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class BlinkLabel extends MultiBitLabel {
    private static final long serialVersionUID = 1L;

    private static final int BLINKING_TIME = 1500; // in ms
    private static final Color ORIGINAL_FOREGROUND_COLOR = Color.BLACK;

    private Color originalBackgroundColor;
    
    private boolean blinkEnabled;

    private String previousBlinkText;

    public BlinkLabel(MultiBitController controller, boolean isLarge) {
        super("", controller);
        
        if (isLarge) {
            setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(3 * MultiBitFrame.MULTIBIT_LARGE_FONT_INCREASE));
        }
        blinkEnabled = false;
        previousBlinkText = "";
        setOpaque(false);
    }

    public void blink(String newLabelText) {
        if (blinkEnabled) {
            if (checkTextHasChanged(newLabelText)) {
                originalBackgroundColor = getBackground();
                setForeground(MultiBitFrame.SELECTION_FOREGROUND_COLOR);
                setBackground(MultiBitFrame.SELECTION_BACKGROUND_COLOR);
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

        public void actionPerformed(ActionEvent e) {
            blinkLabel.setForeground(ORIGINAL_FOREGROUND_COLOR);
            blinkLabel.setBackground(originalBackgroundColor);
            blinkLabel.setOpaque(false);
        }
    }

    public void setBlinkEnabled(boolean blinkEnabled) {
        this.blinkEnabled = blinkEnabled;
    }
}
