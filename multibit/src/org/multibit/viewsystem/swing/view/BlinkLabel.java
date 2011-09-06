package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JLabel;
import javax.swing.Timer;

public class BlinkLabel extends JLabel {
    private static final long serialVersionUID = 1L;

    private static final int BLINKING_TIME = 1500; // in ms
    private static final Color BLINK_COLOR = new Color(175, 120, 23); // dark
                                                                      // gold
    private static final Color ORIGINAL_COLOR = Color.BLACK;

    private boolean blinkEnabled;

    private String previousBlinkText;

    public BlinkLabel() {
        super();
        blinkEnabled = false;
        previousBlinkText = "";
    }

    public void blink(String newLabelText) {
        if (blinkEnabled) {
            if (checkTextHasChanged(newLabelText)) {
                setForeground(BLINK_COLOR);

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
            blinkLabel.setForeground(ORIGINAL_COLOR);
        }
    }

    public void setBlinkEnabled(boolean blinkEnabled) {
        this.blinkEnabled = blinkEnabled;
    }
}
