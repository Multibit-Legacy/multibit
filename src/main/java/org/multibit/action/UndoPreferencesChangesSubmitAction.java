package org.multibit.action;

import java.awt.Font;

import javax.swing.UIManager;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.DataProvider;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;

/**
 * an action that undoes the last change to the preferences panel
 * 
 * @author jim
 * 
 */
public class UndoPreferencesChangesSubmitAction implements Action {
    private MultiBitController controller;

    public UndoPreferencesChangesSubmitAction(MultiBitController controller) {
        this.controller = controller;
    }

    public void execute(DataProvider dataProvider) {
        // get all the previous values and put them into the user preferences;

        String previousFontName = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_NAME);
        String previousFontStyle = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_STYLE);
        int previousFontStyleAsInt = 0;
        try {
            previousFontStyleAsInt = Integer.parseInt(previousFontStyle);
        } catch (NumberFormatException nfe) {
            // just use 0 = plain
        }
        String previousFontSize = (String) controller.getModel().getUserPreference(MultiBitModel.PREVIOUS_FONT_SIZE);
        int previousFontSizeAsInt = MultiBitFrame.MULTIBIT_DEFAULT_FONT_SIZE;
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
