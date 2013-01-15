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
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.dataproviders.PreferencesDataProvider;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.action.PreferencesSubmitAction;

/**
 * This {@link Action} applies changes to the preferences panel.
 */
public class CorePreferencesSubmitAction extends AbstractAction implements PreferencesSubmitAction {

    private static final long serialVersionUID = 1923492460523457765L;

    Controller controller;
    PreferencesDataProvider dataProvider;
    MultiBitFrame mainFrame;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public CorePreferencesSubmitAction(Controller controller, PreferencesDataProvider dataProvider, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.dataProvider = dataProvider;
        this.mainFrame = mainFrame;
    }

    /**
     * Change preferences.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        boolean feeValidationError = false;

        boolean wantToFireDataStructureChanged = false;

        String updateStatusText = "";

        if (dataProvider != null) {
            controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT,
                    dataProvider.getPreviousUndoChangesText());

        }

        String previousLanguageCode = dataProvider.getPreviousUserLanguageCode();
        String newLanguageCode = dataProvider.getNewUserLanguageCode();
        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_USER_LANGUAGE_CODE, previousLanguageCode);

        if (previousLanguageCode != null && !previousLanguageCode.equals(newLanguageCode)) {
            // New language to set on model.
            controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE, newLanguageCode);
            wantToFireDataStructureChanged = true;
        }

        // Open URI - use dialog.
        String openUriDialog = dataProvider.getOpenUriDialog();
        if (openUriDialog != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
        }

        // Open URI - use URI.
        String openUriUseUri = dataProvider.getOpenUriUseUri();
        if (openUriUseUri != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, openUriUseUri);
        }

        // Font data.
        boolean fontHasChanged = false;
        String previousFontName = dataProvider.getPreviousFontName();
        String newFontName = dataProvider.getNewFontName();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_NAME, previousFontName);

        if (newFontName != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_NAME, newFontName);

            if (!newFontName.equals(previousFontName)) {
                fontHasChanged = true;
            }
        }

        String previousFontStyle = dataProvider.getPreviousFontStyle();
        String newFontStyle = dataProvider.getNewFontStyle();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_STYLE, previousFontStyle);

        if (newFontStyle != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_STYLE, newFontStyle);

            if (!newFontStyle.equals(previousFontStyle)) {
                fontHasChanged = true;
            }
        }

        String previousFontSize = dataProvider.getPreviousFontSize();
        String newFontSize = dataProvider.getNewFontSize();

        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_FONT_SIZE, previousFontSize);

        if (newFontSize != null) {
            controller.getModel().setUserPreference(MultiBitModel.FONT_SIZE, newFontSize);

            if (!newFontSize.equals(previousFontSize)) {
                fontHasChanged = true;
            }
        }
        
        // Look and feel.
        boolean lookAndFeelHasChanged = false;
        String previousLookAndFeel = dataProvider.getPreviousLookAndFeel();
        String newLookAndFeel = dataProvider.getNewLookAndFeel();

        controller.getModel().setUserPreference(MultiBitModel.LOOK_AND_FEEL, previousLookAndFeel);

        if (newLookAndFeel != null && (!newLookAndFeel.equals(previousLookAndFeel) || !newLookAndFeel.equals(UIManager.getLookAndFeel().getName()))) {
            controller.getModel().setUserPreference(MultiBitModel.LOOK_AND_FEEL, newLookAndFeel);

            lookAndFeelHasChanged = true;
        }

        // Can undo.
        controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "true");

        
        if (fontHasChanged) {
            Font newFont = dataProvider.getSelectedFont();
            FontSizer.INSTANCE.initialise(controller);
            if (newFont != null) {
                UIManager.put("ToolTip.font", newFont);
            }

            wantToFireDataStructureChanged = true;
        }

        if (wantToFireDataStructureChanged) {
            // Redo everything.
            controller.fireDataStructureChanged();
        }
        
        if (lookAndFeelHasChanged) {
            try {
                if (MultiBitModel.SYSTEM_LOOK_AND_FEEL.equals(newLookAndFeel)) {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
                else {
                    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                        if (newLookAndFeel.equalsIgnoreCase(info.getName())) {
                            UIManager.setLookAndFeel(info.getClassName());
                            break;
                        }
                    }
                }
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            } catch (InstantiationException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (UnsupportedLookAndFeelException e) {
                e.printStackTrace();
            }

            ColorAndFontConstants.init();
            controller.fireDataStructureChanged();
            SwingUtilities.updateComponentTreeUI(mainFrame);
        }

        // Return to the same view.
        controller.displayView(controller.getCurrentView());
        
        if (feeValidationError) {
            MessageManager.INSTANCE.addMessage(new Message(updateStatusText));
        } else {
            // Clear any previous validation error.
            MessageManager.INSTANCE.addMessage(new Message(" "));
        }
    }
}