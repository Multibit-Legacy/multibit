package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;

public class MultiBitTextField extends JTextField {

    private static final long serialVersionUID = 8706099337840832271L;
   
    public MultiBitTextField(String text, int width, MultiBitController controller) {
        super(text, width);
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
    }
}
