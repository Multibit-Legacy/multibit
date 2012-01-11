package org.multibit.viewsystem.swing.action;

import java.awt.Font;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.view.ShowPreferencesPanel;
import org.multibit.viewsystem.swing.view.components.JFontChooser;

/**
 * This {@link Action} represents the choose font action
 */
public class ChooseFontAction extends AbstractAction {

    private static final long serialVersionUID = 114359435465057705L;

    private MultiBitController controller;
    private ShowPreferencesPanel showPreferencesPanel;

    /**
     * Creates a new {@link ChooseFontAction}.
     */
    public ChooseFontAction(MultiBitController controller, ShowPreferencesPanel showPreferencesPanel, ImageIcon icon) {
        super(controller.getLocaliser().getString("fontChooser.text"), icon);
        this.controller = controller;
        this.showPreferencesPanel = showPreferencesPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("fontChooser.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("fontChooser.mnemonicKey"));
    }

    /**
     * show font chooser dialog
     */
    public void actionPerformed(ActionEvent e) {
        JFontChooser fontChooser = new JFontChooser(controller);
        fontChooser.setSelectedFont(showPreferencesPanel.getSelectedFont());
        int result = fontChooser.showDialog(showPreferencesPanel);
        if (result == JFontChooser.OK_OPTION) {
            Font font = fontChooser.getSelectedFont();
            System.out.println("Selected Font : " + font);
            showPreferencesPanel.setSelectedFont(font);
        }
    }
}