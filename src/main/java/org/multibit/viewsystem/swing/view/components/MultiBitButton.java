package org.multibit.viewsystem.swing.view.components;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.controller.MultiBitController;

/**
 * button used in MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitButton extends JButton {

    private static final long serialVersionUID = 5632457290711815650L;
    
    public MultiBitButton(Action action, MultiBitController controller) {
        super(action);
              
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        setOpaque(false);
        setRolloverEnabled(true);
    }
    
    public MultiBitButton(String label) {
        super(label);
        
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        setOpaque(false);
        setRolloverEnabled(true);
    }
}
