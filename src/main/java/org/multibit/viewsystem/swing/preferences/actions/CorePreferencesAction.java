/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham <da2ce7@gmail.com>.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.viewsystem.swing.preferences.actions;

import com.google.bitcoin.core.Utils;
import java.awt.Font;
import java.math.BigInteger;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import org.multibit.controller.core.CoreController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.core.CoreModel;
import org.multibit.viewsystem.dataproviders.core.CorePreferencesDataProvider;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesAction;
import org.multibit.viewsystem.swing.preferences.PreferencesPanel;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class CorePreferencesAction extends AbstractPreferencesAction<CorePreferencesDataProvider, CoreController> {

    public CorePreferencesAction(CorePreferencesDataProvider corePreferencesDataProvider, CoreController coreController) {
        super(corePreferencesDataProvider, coreController);
    }

    @Override
    public Boolean Submit() {

        boolean wantToFireDataStructureChanged = false;

        if (dataProvider != null) {
            controller.getModel().setUserPreference(CoreModel.PREVIOUS_UNDO_CHANGES_TEXT,
                    dataProvider.getPreviousUndoChangesText());

            String previousLanguageCode = dataProvider.getPreviousUserLanguageCode();
            String newLanguageCode = dataProvider.getNewUserLanguageCode();
            controller.getModel().setUserPreference(CoreModel.PREVIOUS_USER_LANGUAGE_CODE, previousLanguageCode);

            if (previousLanguageCode != null && !previousLanguageCode.equals(newLanguageCode)) {
                // New language to set on model.
                controller.getModel().setUserPreference(CoreModel.USER_LANGUAGE_CODE, newLanguageCode);
                wantToFireDataStructureChanged = true;
            }

            // Open URI - use dialog.
            String openUriDialog = dataProvider.getOpenUriDialog();
            if (openUriDialog != null) {
                controller.getModel().setUserPreference(BitcoinModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
            }

            // Open URI - use URI.
            String openUriUseUri = dataProvider.getOpenUriUseUri();
            if (openUriUseUri != null) {
                controller.getModel().setUserPreference(BitcoinModel.OPEN_URI_USE_URI, openUriUseUri);
            }

            // Font data.
            boolean fontHasChanged = false;
            String previousFontName = dataProvider.getPreviousFontName();
            String newFontName = dataProvider.getNewFontName();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_NAME, previousFontName);

            if (newFontName != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_NAME, newFontName);

                if (!newFontName.equals(previousFontName)) {
                    fontHasChanged = true;
                }
            }

            String previousFontStyle = dataProvider.getPreviousFontStyle();
            String newFontStyle = dataProvider.getNewFontStyle();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_STYLE, previousFontStyle);

            if (newFontStyle != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_STYLE, newFontStyle);

                if (!newFontStyle.equals(previousFontStyle)) {
                    fontHasChanged = true;
                }
            }

            String previousFontSize = dataProvider.getPreviousFontSize();
            String newFontSize = dataProvider.getNewFontSize();

            controller.getModel().setUserPreference(CoreModel.PREVIOUS_FONT_SIZE, previousFontSize);

            if (newFontSize != null) {
                controller.getModel().setUserPreference(CoreModel.FONT_SIZE, newFontSize);

                if (!newFontSize.equals(previousFontSize)) {
                    fontHasChanged = true;
                }
            }

            // Look and feel.
            boolean lookAndFeelHasChanged = false;
            String previousLookAndFeel = dataProvider.getPreviousLookAndFeel();
            String newLookAndFeel = dataProvider.getNewLookAndFeel();

            controller.getModel().setUserPreference(CoreModel.LOOK_AND_FEEL, previousLookAndFeel);

            if (newLookAndFeel != null
                    && (!newLookAndFeel.equals(previousLookAndFeel) && !newLookAndFeel.equals(UIManager.getLookAndFeel().getName()))) {
                controller.getModel().setUserPreference(CoreModel.LOOK_AND_FEEL, newLookAndFeel);

                lookAndFeelHasChanged = true;
            }


            if (lookAndFeelHasChanged) {
                try {
                    if (CoreModel.SYSTEM_LOOK_AND_FEEL.equals(newLookAndFeel)) {
                        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    } else {
                        for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
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
            }
            Font newFont = dataProvider.getSelectedFont();
            if (newFont != null) {
                UIManager.put("ToolTip.font", newFont);
            }

            if (lookAndFeelHasChanged || fontHasChanged) {
                wantToFireDataStructureChanged = true;
            }
        }
        return wantToFireDataStructureChanged;
    }

    @Override
    public Boolean Undo() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
