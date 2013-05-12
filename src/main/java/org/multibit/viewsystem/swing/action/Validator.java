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

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.exchange.CurrencyConverterResult;
import org.multibit.model.MultiBitModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * A class to validate String addresses and amounts.
 * TODO - this should create a validation state object and have a getter
 * 
 * @author jim
 * 
 */
public class Validator {
    private static final Logger log = LoggerFactory.getLogger(Validator.class);

    private final Controller controller;
    private final BitcoinController bitcoinController;

    public Validator(BitcoinController bitcoinController) {
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
    }

    /**
     * Validate a String address and amount.
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
     * Validate a String address.
     * 
     * @param address
     * @return
     */
    public boolean validate(String address) {
        clearValidationState();
        return validateAddress(address);
    }

    private boolean validateAmount(String amount) {
        // Copy amount to wallet preferences.
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE, amount);

        Boolean amountValidatesOk = Boolean.TRUE;

        Boolean amountIsInvalid = Boolean.FALSE;
        Boolean notEnoughFunds = Boolean.FALSE;
        Boolean amountIsMissing = Boolean.FALSE;
        Boolean amountIsNegativeOrZero = Boolean.FALSE;

        // See if the amount is missing.
        if (amount == null || "".equals(amount) || amount.trim().length() == 0) {
            amountIsMissing = Boolean.TRUE;
            amountValidatesOk = Boolean.FALSE;
        } else {
            // See if the amount is a number.
            BigInteger amountBigInteger = null;
            try {
                CurrencyConverterResult converterResult = CurrencyConverter.INSTANCE.parseToBTCNotLocalised(amount);
                if (converterResult.isBtcMoneyValid()) {
                    // Parses ok.
                    amountBigInteger = converterResult.getBtcMoney().getAmount().toBigInteger();
                } else {
                    amountIsInvalid = Boolean.TRUE;
                    amountValidatesOk = Boolean.FALSE;
                }
            } catch (NumberFormatException nfe) {
                amountValidatesOk = Boolean.FALSE;
                amountIsInvalid = Boolean.TRUE;
            } catch (ArithmeticException ae) {
                amountValidatesOk = Boolean.FALSE;
                amountIsInvalid = Boolean.TRUE;
            }

            // See if the amount is negative or zero.
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
                    BigInteger availableBalance = this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.AVAILABLE);
                    BigInteger estimatedBalance = this.bitcoinController.getModel().getActiveWallet().getBalance(BalanceType.ESTIMATED);
                    
                    log.debug("Amount = " + amountBigInteger.toString() + ", fee = " + feeBigInteger.toString() 
                            + ", totalSpend = " + totalSpend.toString() + ", availableBalance = " + availableBalance.toString() + ", estimatedBalance = " + estimatedBalance.toString());
                    if (totalSpend.compareTo(availableBalance) > 0) {
                        // Not enough funds.
                        amountValidatesOk = Boolean.FALSE;
                        notEnoughFunds = Boolean.TRUE;
                    }
                }
            }
        }
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, amountIsMissing.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO,
                amountIsNegativeOrZero.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, amountIsInvalid.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, notEnoughFunds.toString());

        return amountValidatesOk.booleanValue();
    }

    private boolean validateAddress(String address) {
        Boolean addressIsInvalid = Boolean.TRUE;

        if (address != null && !address.isEmpty()) {
            // Copy address to wallet preferences.
            this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, address);

            try {
                new Address(this.bitcoinController.getModel().getNetworkParameters(), address);
                addressIsInvalid = Boolean.FALSE;
            } catch (AddressFormatException afe) {
                // Carry on.
            } catch (java.lang.StringIndexOutOfBoundsException e) {
                // Carry on.
            }
        } else {
            this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, "");
        }
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, addressIsInvalid.toString());

        return !addressIsInvalid.booleanValue();
    }

    public void clearValidationState() {
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, Boolean.FALSE.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO, Boolean.FALSE.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, Boolean.FALSE.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, Boolean.FALSE.toString());
        this.bitcoinController.getModel().setActiveWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, Boolean.FALSE.toString());
    }
}
