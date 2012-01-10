package org.multibit.viewsystem.swing.view.components;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * button used in MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitButton extends JButton {

    private static final long serialVersionUID = 5632457290711815650L;

    public MultiBitButton(Action action) {
        super(action);
        
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
        setOpaque(false);
        setRolloverEnabled(true);
    }
    
    public MultiBitButton(String label) {
        super(label);
        
        FontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_NORMAL_FONT_SIZE);
        setOpaque(false);
        setRolloverEnabled(true);
    }
}
