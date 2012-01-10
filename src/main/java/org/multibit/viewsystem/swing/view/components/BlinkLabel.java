package org.multibit.viewsystem.swing.view.components;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class BlinkLabel extends MultiBitLabel {
    private static final long serialVersionUID = 1L;

    private static final int BLINKING_TIME = 1500; // in ms
    private static final Color ORIGINAL_FOREGROUND_COLOR = Color.BLACK;

    private Color originalBackgroundColor;
    
    private boolean blinkEnabled;

    private String previousBlinkText;

    public BlinkLabel(MultiBitController controller) {
        super("", controller);
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
