package org.multibit.viewsystem.swing.view.components;

import javax.swing.JLabel;

import org.multibit.controller.MultiBitController;

public class MultiBitLabel extends JLabel {

    private static final long serialVersionUID = -3434455262992702604L;

    public MultiBitLabel(String labelText, int alignment, MultiBitController controller) {
        super(labelText, alignment);
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
    }

    public MultiBitLabel(String labelText, MultiBitController controller) {
        super(labelText);
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
    }
}
