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

import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * a class to validate String addresses and amounts
 * TODO - this should create a validation state object and have a getter
 * 
 * @author jim
 * 
 */
public class Validator {

    private MultiBitController controller;

    public Validator(MultiBitController controller) {
        this.controller = controller;
    }

    /**
     * Validate a String address and amount
     * 
     * @param address
     * @param amount
     * @return
     */
    public boolean validate(String address, String amount) {
        clearValidationState();

        boolean validAddress = validateAddress(address);
        boolean validAmount = validateAmount(amount);
        return validAddress && validAmount;
    }

    /**
     * Validate a String address
     * 
     * @param address
     * @return
     */
    public boolean validate(String address) {
        clearValidationState();
        return validateAddress(address);
    }

    private boolean validateAmount(String amount) {
        // copy amount to wallet preferences
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE, amount);

        Boolean amountValidatesOk = Boolean.TRUE;

        Boolean amountIsInvalid = Boolean.FALSE;
        Boolean notEnoughFunds = Boolean.FALSE;
        Boolean amountIsMissing = Boolean.FALSE;
        Boolean amountIsNegativeOrZero = Boolean.FALSE;

        // see if the amount is missing
        if (amount == null || "".equals(amount)) {
            amountIsMissing = Boolean.TRUE;
            amountValidatesOk = Boolean.FALSE;
        } else {
            // see if the amount is a number
            BigInteger amountBigInteger = null;
            try {
                amountBigInteger = Utils.toNanoCoins(amount);
                if (amountBigInteger == null) {
                    amountIsMissing = Boolean.TRUE;
                    amountValidatesOk = Boolean.FALSE;
                }
            } catch (NumberFormatException nfe) {
                amountValidatesOk = Boolean.FALSE;
                amountIsInvalid = Boolean.TRUE;
            } catch (ArithmeticException ae) {
                amountValidatesOk = Boolean.FALSE;
                amountIsInvalid = Boolean.TRUE;
            }

            // see if the amount is negative or zero
            if (amountValidatesOk.booleanValue()) {
                if (amountBigInteger.compareTo(BigInteger.ZERO) <= 0) {
                    amountValidatesOk = Boolean.FALSE;
                    amountIsNegativeOrZero = Boolean.TRUE;
                } else {
                    String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
                    BigInteger feeBigInteger;
                    if (fee == null || fee.isEmpty()) {
                        feeBigInteger = MultiBitModel.SEND_FEE_DEFAULT;
                    } else {
                        feeBigInteger = Utils.toNanoCoins(fee);
                    }
                    BigInteger totalSpend = amountBigInteger.add(feeBigInteger);
                    BigInteger availableBalance = controller.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE);
                    if (totalSpend.compareTo(availableBalance) > 0) {
                        // not enough funds
                        amountValidatesOk = Boolean.FALSE;
                        notEnoughFunds = Boolean.TRUE;
                    }
                }
            }
        }
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, amountIsMissing.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO,
                amountIsNegativeOrZero.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, amountIsInvalid.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, notEnoughFunds.toString());

        return amountValidatesOk.booleanValue();
    }

    private boolean validateAddress(String address) {
        Boolean addressIsInvalid = Boolean.TRUE;

        if (address != null && !address.isEmpty()) {
            // copy address to wallet preferences
            controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, address);

            try {
                new Address(controller.getModel().getNetworkParameters(), address);
                addressIsInvalid = Boolean.FALSE;
            } catch (AddressFormatException afe) {
                // carry on
            } catch (java.lang.StringIndexOutOfBoundsException e) {
                // carry on
            }
        } else {
            controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, "");
        }
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, addressIsInvalid.toString());

        return !addressIsInvalid.booleanValue();
    }

    public void clearValidationState() {
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, Boolean.FALSE.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO, Boolean.FALSE.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, Boolean.FALSE.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, Boolean.FALSE.toString());
        controller.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, Boolean.FALSE.toString());
    }
}
