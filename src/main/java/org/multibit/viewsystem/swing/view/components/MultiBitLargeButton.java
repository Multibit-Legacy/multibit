package org.multibit.viewsystem.swing.view.components;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * button used in toolbar on MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitLargeButton extends JButton {

    private static final long serialVersionUID = 5674557290711815650L;

    public MultiBitLargeButton(Action action) {
        super(action);
        
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE);
        setOpaque(false);
        setRolloverEnabled(true);
    }
}
