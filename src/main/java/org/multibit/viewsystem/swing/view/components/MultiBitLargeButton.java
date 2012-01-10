package org.multibit.viewsystem.swing.view.components;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * button used in toolbar on MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitLargeButton extends JButton {

    private static final long serialVersionUID = 5674557290711815650L;

    private MultiBitController controller;
    
    public MultiBitLargeButton(Action action, MultiBitController controller) {
        super(action);
    
        this.controller = controller;
        
        setAdjustedFont();
        setOpaque(false);
        setRolloverEnabled(true);
    }
    
    
    private void setAdjustedFont() {
        String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
        FontSizer fontSizer = new FontSizer(controller);
        if (fontSizeString == null || "".equals(fontSizeString)) {
            fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE + MultiBitFrame.MULTIBIT_LARGE_FONT_INCREASE);
        } else {
            try {
                fontSizer.setAdjustedFont(this, Integer.parseInt(fontSizeString) + MultiBitFrame.MULTIBIT_LARGE_FONT_INCREASE);
            } catch (NumberFormatException nfe) {
                fontSizer.setAdjustedFont(this, MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE + MultiBitFrame.MULTIBIT_LARGE_FONT_INCREASE);                
            }
        }
    }
}
