package org.multibit.viewsystem.swing.view.components;

import java.text.ParseException;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * button used in MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitButton extends JButton {

    private static final long serialVersionUID = 5632457290711815650L;

    private MultiBitController controller;
    
    public MultiBitButton(Action action, MultiBitController controller) {
        super(action);
        
        this.controller = controller;
        
        setAdjustedFont();
        setOpaque(false);
        setRolloverEnabled(true);
    }
    
    public MultiBitButton(String label) {
        super(label);
        
        setAdjustedFont();
        setOpaque(false);
        setRolloverEnabled(true);
    }
    
    private void setAdjustedFont() {
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        FontSizer fontSizer = new FontSizer(controller);
        if (fontSizeString == null || "".equals(fontSizeString)) {
            fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE);
        } else {
            try {
                fontSizer.setAdjustedFont(this, Integer.parseInt(fontSizeString));
            } catch (NumberFormatException nfe) {
                fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE);                
            }
        }
    }
}
