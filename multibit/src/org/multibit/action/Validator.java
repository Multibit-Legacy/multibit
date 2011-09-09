package org.multibit.action;

import java.math.BigInteger;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;

import com.google.bitcoin.core.Address;
import com.google.bitcoin.core.AddressFormatException;
import com.google.bitcoin.core.Utils;
import com.google.bitcoin.core.Wallet.BalanceType;

/**
 * a class to validate String addresses and amounts
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
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE, amount);

        Boolean amountValidatesOk = Boolean.TRUE;

        Boolean amountIsInvalid = Boolean.FALSE;
        Boolean notEnoughFunds = Boolean.FALSE;
        Boolean notEnoughFundsButEstimatedOk = Boolean.FALSE;
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
                amountValidatesOk = false;
                amountIsInvalid = Boolean.TRUE;
            } catch (ArithmeticException ae) {
                amountValidatesOk = false;
                amountIsInvalid = Boolean.TRUE;
            }

            // see if the amount is negative or zero
            if (amountValidatesOk) {
                if (amountBigInteger.compareTo(BigInteger.ZERO) <= 0) {
                    amountValidatesOk = Boolean.FALSE;
                    amountIsNegativeOrZero = Boolean.TRUE;
                } else {
                    String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
                    BigInteger feeBigInteger;
                    if (fee == null || fee == "") {
                        feeBigInteger = MultiBitModel.SEND_MINIMUM_FEE;
                    } else {
                        feeBigInteger = Utils.toNanoCoins(fee);
                    }
                    BigInteger totalSpend = amountBigInteger.add(feeBigInteger);
                    BigInteger availableBalance = controller.getModel().getWallet().getBalance(BalanceType.AVAILABLE);
                    if (totalSpend.compareTo(availableBalance) <= 0) {
                        // there is enough money
                    } else {
                        // not enough funds - check if estimated balance
                        // suggests
                        // there
                        // is (different error message)
                        amountValidatesOk = false;
                        if (totalSpend.compareTo(controller.getModel().getWallet().getBalance(BalanceType.ESTIMATED)) <= 0) {
                            // the UI shows enough money but there is not
                            // enough
                            // available
                            notEnoughFundsButEstimatedOk = Boolean.TRUE;
                        } else {
                            // the total spend is less than both the
                            // estimated
                            // and
                            // available - not confusing
                            notEnoughFunds = Boolean.TRUE;
                        }
                    }
                }
            }
        }
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, amountIsMissing.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO,
                amountIsNegativeOrZero.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, amountIsInvalid.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, notEnoughFunds.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS_BUT_ESTIMATED_OK,
                notEnoughFundsButEstimatedOk.toString());

        return amountValidatesOk;
    }

    private boolean validateAddress(String address) {
        Boolean addressIsInvalid = Boolean.TRUE;

        if (address != null) {
            // copy address to wallet preferences
            controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, address);

            try {
                Address parsedAddress = new Address(controller.getMultiBitService().getNetworkParameters(), address);
                addressIsInvalid = Boolean.FALSE;
            } catch (AddressFormatException afe) {
                // carry on
            }
        } else {
            controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, "");
        }
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, addressIsInvalid.toString());

        return !addressIsInvalid.booleanValue();
    }

    public void clearValidationState() {
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_MISSING, Boolean.FALSE.toString());
        controller.getModel()
                .setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_NEGATIVE_OR_ZERO, Boolean.FALSE.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, Boolean.FALSE.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, Boolean.FALSE.toString());
        controller.getModel().setWalletPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, Boolean.FALSE.toString());
    }
}
