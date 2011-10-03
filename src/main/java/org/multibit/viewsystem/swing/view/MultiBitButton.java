package org.multibit.viewsystem.swing.view;

import java.awt.Font;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * button used in toolbar on MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitButton extends JButton {

    private static final long serialVersionUID = 5674557290711815650L;

    public MultiBitButton(Action action) {
        super(action);
        
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE, MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE);
        setFont(font);
        setOpaque(false);
        setRolloverEnabled(true);
    }
}
