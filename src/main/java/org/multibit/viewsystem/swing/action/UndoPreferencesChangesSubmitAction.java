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
import javax.swing.UIManager;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.view.components.FontSizer;

/**
 * This {@link Action} undoes the last changes made to the preferences panel
 */
public class UndoPreferencesChangesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492412423457765L;

    private MultiBitController controller;
    private DataProvider dataProvider;

    /**
     * Creates a new {@link UndoPreferencesChangesSubmitAction}.
     */
    public UndoPreferencesChangesSubmitAction(MultiBitController controller, DataProvider dataProvider) {
        super(controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.text"));
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("undoPreferencesChangesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("undoPreferencesChangesSubmitAction.mnemonicKey"));
    }

    /**
     * get all the previous changes and undo them
     */
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
            previousFontSizeAsInt = Integer.parseInt(previousFontStyle);
        } catch (NumberFormatException nfe) {
            // just use default
        }

        controller.getModel().setUserPreference(MultiBitModel.SEND_FEE,
                (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_SEND_FEE));
        controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE,
                (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_USER_LANGUAGE_CODE));
        controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG,
                (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_OPEN_URI_SHOW_DIALOG));
        controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI,
                (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_OPEN_URI_USE_URI));
        controller.getModel().setUserPreference(MultiBitModel.FONT_NAME, previousFontName);
        controller.getModel().setUserPreference(MultiBitModel.FONT_STYLE, previousFontStyle);
        controller.getModel().setUserPreference(MultiBitModel.FONT_SIZE, previousFontSize);
        controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "false");

        // return to the same view but fire a language change to reset
        // everything
        FontSizer.INSTANCE.initialise(controller);
        UIManager.put("ToolTip.font", new Font(previousFontName, previousFontStyleAsInt, previousFontSizeAsInt));

        controller.fireLanguageChanged();
        controller.setActionForwardToSiblingOfParent(ActionForward.FORWARD_TO_SAME);
    }
}