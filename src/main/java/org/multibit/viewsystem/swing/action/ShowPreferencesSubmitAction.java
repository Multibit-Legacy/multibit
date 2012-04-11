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
import java.math.BigInteger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.UIManager;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.dataproviders.PreferencesDataProvider;
import org.multibit.viewsystem.swing.view.components.FontSizer;

import com.google.bitcoin.core.Utils;

/**
 * This {@link Action} applies changes to the preferences panel
 */
public class ShowPreferencesSubmitAction extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;

    private MultiBitController controller;
    private PreferencesDataProvider dataProvider;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public ShowPreferencesSubmitAction(MultiBitController controller, PreferencesDataProvider dataProvider, Icon icon) {
        super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);
        this.controller = controller;
        this.dataProvider = dataProvider;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * change preferences
     */
    public void actionPerformed(ActionEvent event) {
        boolean feeValidationError = false;
        String updateStatusText = "";

        if (dataProvider != null) {
            controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_UNDO_CHANGES_TEXT,
                    dataProvider.getPreviousUndoChangesText());

            String previousSendFee = dataProvider.getPreviousSendFee();
            String newSendFee = dataProvider.getNewSendFee();
            controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_SEND_FEE, previousSendFee);
            // check fee is set
            if (newSendFee == null || "".equals(newSendFee)) {
                // fee must be set validation error
                controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                feeValidationError = true;
                updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.aFeeMustBeSet");
            }

            if (!feeValidationError) {
                try {
                    // check fee is a number
                    BigInteger feeAsBigInteger = Utils.toNanoCoins(newSendFee);

                    // check fee is at least the minimum fee
                    if (feeAsBigInteger.compareTo(MultiBitModel.SEND_MINIMUM_FEE) < 0) {
                        feeValidationError = true;
                        updateStatusText = controller.getLocaliser().getString(
                                "showPreferencesPanel.feeCannotBeSmallerThanMinimumFee");
                    } else {
                        // fee is ok
                        controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, newSendFee);
                    }
                } catch (NumberFormatException nfe) {
                    // recycle the old fee and set status message
                    controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[] { newSendFee });
                } catch (ArithmeticException ae) {
                    // recycle the old fee and set status message
                    controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[] { newSendFee });
                }
            }
        }

        String previousLanguageCode = dataProvider.getPreviousUserLanguageCode();
        String newLanguageCode = dataProvider.getNewUserLanguageCode();
        controller.getModel().setUserPreference(MultiBitModel.PREVIOUS_USER_LANGUAGE_CODE, previousLanguageCode);

        if (previousLanguageCode != null && !previousLanguageCode.equals(newLanguageCode)) {
            // new language to set on model
            controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE, newLanguageCode);
            controller.fireDataStructureChanged();
        }

        // open URI - use dialog
        String openUriDialog = dataProvider.getOpenUriDialog();
        if (openUriDialog != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_SHOW_DIALOG, openUriDialog);
        }

        // open URI - use URI
        String openUriUseUri = dataProvider.getOpenUriUseUri();
        if (openUriUseUri != null) {
            controller.getModel().setUserPreference(MultiBitModel.OPEN_URI_USE_URI, openUriUseUri);
        }

        // font data
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

        // currency ticker
        // show rate (or bidAndAsk)
        String previousShowRate = new Boolean(dataProvider.getPreviousShowRate()).toString();
        String newShowRate = new Boolean(dataProvider.getNewShowRate()).toString();
        if (previousShowRate != null && !previousShowRate.equals(newShowRate)) {
            // new show rate is set on model
            controller.getModel().setUserPreference(MultiBitModel.TICKER_SHOW_RATE, newShowRate);
            controller.fireDataStructureChanged();
        }

        // can undo
        controller.getModel().setUserPreference(MultiBitModel.CAN_UNDO_PREFERENCES_CHANGES, "true");

        if (fontHasChanged) {
            Font newFont = dataProvider.getSelectedFont();
            FontSizer.INSTANCE.initialise(controller);
            if (newFont != null) {
                UIManager.put("ToolTip.font", newFont);
            }

            // redo everything
            controller.fireDataStructureChanged();
        }

        // return to the same view
        controller.displayView(controller.getCurrentView());

        if (feeValidationError) {
            controller.updateStatusLabel(updateStatusText);
        } else {
            // clear any previous validation error
            controller.updateStatusLabel(" ");
        }
    }
}