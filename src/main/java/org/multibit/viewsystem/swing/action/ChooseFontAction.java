/**
 * Copyright 2012 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.multibit.viewsystem.swing.action;

import java.awt.Font;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.ShowPreferencesPanel;
import org.multibit.viewsystem.swing.view.components.JFontChooser;

/**
 * This {@link Action} represents the choose font action.
 */
public class ChooseFontAction extends AbstractAction {

    private static final long serialVersionUID = 114359435465057705L;

    private Controller controller;
    private ShowPreferencesPanel showPreferencesPanel;

    /**
     * Creates a new {@link ChooseFontAction}.
     */
    public ChooseFontAction(Controller controller, ShowPreferencesPanel showPreferencesPanel, ImageIcon icon) {
        super(controller.getLocaliser().getString("fontChooser.text"), icon);
        this.controller = controller;
        this.showPreferencesPanel = showPreferencesPanel;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("fontChooser.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("fontChooser.mnemonicKey"));
    }

    /**
     * Show font chooser dialog.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        JFontChooser fontChooser = new JFontChooser(controller);
        fontChooser.setSelectedFont(showPreferencesPanel.getSelectedFont());
        int result = fontChooser.showDialog(showPreferencesPanel);
        Font font = fontChooser.getSelectedFont();
        if (result == JFontChooser.OK_OPTION) {
             showPreferencesPanel.setSelectedFont(font);
        }
    }
}