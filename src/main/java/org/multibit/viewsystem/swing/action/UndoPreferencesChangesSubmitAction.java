/**
 * Copyright 2011 multibit.org
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
import javax.swing.UIManager;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.view.components.FontSizer;

/**
 * This {@link Action} undoes the last language and font changes made to the
 * preferences panel
 */
public class UndoPreferencesChangesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492412423457765L;

    private MultiBitController controller;

    /**
     * Creates a new {@link UndoPreferencesChangesSubmitAction}.
     */
    public UndoPreferencesChangesSubmitAction(MultiBitController controller, ImageIcon icon) {
        super(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text"), icon);
        this.controller = controller;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("undoPreferencesChangesSubmitAction.mnemonicKey"));
    }

    /**
     * Get the previous language and font changes and undo them
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        String previousFontName = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_NAME);
        String previousFontStyle = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_STYLE);
        int previousFontStyleAsInt = 0;
        try {
            previousFontStyleAsInt = Integer.parseInt(previousFontStyle);
        } catch (NumberFormatException nfe) {
            // just use 0 = plain
        }
        String previousFontSize = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_SIZE);
        int previousFontSizeAsInt = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        try {
            previousFontSizeAsInt = Integer.parseInt(previousFontSize);
        } catch (NumberFormatException nfe) {
            // just use default
        }

        controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE,
                (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_USER_LANGUAGE_CODE));
        controller.getModel().setUserPreference(MultiBitModel.FONT_NAME, previousFontName);
        controller.getModel().setUserPreference(MultiBitModel.FONT_STYLE, previousFontStyle);
        controller.getModel().setUserPreference(MultiBitModel.FONT_SIZE, previousFontSize);
        controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "false");

        // return to the same view but fire data structure change to reset
        // everything
        FontSizer.INSTANCE.initialise(controller);
        UIManager.put("ToolTip.font", new Font(previousFontName, previousFontStyleAsInt, previousFontSizeAsInt));

        controller.fireDataStructureChanged();
        controller.displayView(View.PREFERENCES_VIEW);
    }
}