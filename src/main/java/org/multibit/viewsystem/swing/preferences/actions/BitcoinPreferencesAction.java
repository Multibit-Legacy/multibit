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
import java.math.BigInteger;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.viewsystem.dataproviders.bitcoin.BitcoinPreferencesDataProvider;
import org.multibit.viewsystem.swing.preferences.AbstractPreferencesAction;
import org.multibit.viewsystem.swing.preferences.PreferencesPanel;
import org.multibit.viewsystem.swing.preferences.modules.BitcoinPreferencesModule;

/**
 *
 * @author Cameron Garnham <da2ce7@gmail.com>
 */
public class BitcoinPreferencesAction extends AbstractPreferencesAction<BitcoinPreferencesDataProvider, BitcoinController> {

    public BitcoinPreferencesAction(BitcoinPreferencesDataProvider bitcoinPreferencesDataProvider, BitcoinController bitcoinController) {
        super(bitcoinPreferencesDataProvider, bitcoinController);
    }

    @Override
    public Boolean Submit() {

        boolean feeValidationError = false;
        String updateStatusText = "";

        if (dataProvider != null) {

            String previousSendFee = dataProvider.getPreviousSendFee();
            String newSendFee = dataProvider.getNewSendFee();
            controller.getModel().setUserPreference(BitcoinModel.PREVIOUS_SEND_FEE, previousSendFee);

            // Check fee is set.
            if (newSendFee == null || "".equals(newSendFee)) {
                // Fee must be set validation error.
                controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
                feeValidationError = true;
                updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.aFeeMustBeSet");
            }

            if (!feeValidationError) {
                if (newSendFee.startsWith(BitcoinPreferencesModule.UNPARSEABLE_FEE)) {
                    String newSendFeeTrimmed = newSendFee.substring(BitcoinPreferencesModule.UNPARSEABLE_FEE.length() + 1);
                    // Recycle the old fee and set status message.
                    controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[]{newSendFeeTrimmed});
                }
            }

            if (!feeValidationError) {
                try {
                    // Check fee is a number.
                    BigInteger feeAsBigInteger = Utils.toNanoCoins(newSendFee);

                    // Check fee is at least the minimum fee.
                    if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MINIMUM_FEE) < 0) {
                        feeValidationError = true;
                        updateStatusText = controller.getLocaliser().getString(
                                "showPreferencesPanel.feeCannotBeSmallerThanMinimumFee");
                    } else {
                        if (feeAsBigInteger.compareTo(BitcoinModel.SEND_MAXIMUM_FEE) >= 0) {
                            feeValidationError = true;
                            updateStatusText = controller.getLocaliser().getString(
                                    "showPreferencesPanel.feeCannotBeGreaterThanMaximumFee");
                        } else {
                            // Fee is ok.
                            controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, newSendFee);
                        }
                    }
                } catch (NumberFormatException nfe) {
                    // Recycle the old fee and set status message.
                    controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[]{newSendFee});
                } catch (ArithmeticException ae) {
                    // Recycle the old fee and set status message.
                    controller.getModel().setUserPreference(BitcoinModel.SEND_FEE, previousSendFee);
                    feeValidationError = true;
                    updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee",
                            new Object[]{newSendFee});
                }
            }

            if (feeValidationError) {
                MessageManager.INSTANCE.addMessage(new Message(updateStatusText, false));
            } else {
                // Clear any previous validation error.
                MessageManager.INSTANCE.addMessage(new Message(" "));
            }
        }
        return false;
    }

        @Override
        public Boolean Undo() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }
