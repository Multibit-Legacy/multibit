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
        // copy amount to user preferences
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_AMOUNT_VALUE, amount);

        // see if the amount is a number
        Boolean amountIsInvalid = Boolean.TRUE;
        BigInteger amountBigInteger = null;
        try {
            amountBigInteger = Utils.toNanoCoins(amount);
            amountIsInvalid = Boolean.FALSE;
        } catch (NumberFormatException nfe) {
            // carry on
        } catch (ArithmeticException ae) {
            // carry on
        }

        Boolean notEnoughFunds = Boolean.FALSE;
        Boolean notEnoughFundsButEstimatedOk = Boolean.FALSE;
        if (amountBigInteger != null) {
            String fee = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);
            BigInteger feeBigInteger;
            if (fee == null || fee == "") {
                feeBigInteger = MultiBitModel.SEND_FEE_DEFAULT;
            } else {
                feeBigInteger = Utils.toNanoCoins(fee);
            }
            BigInteger totalSpend = amountBigInteger.add(feeBigInteger);
            BigInteger availableBalance = controller.getModel().getWallet().getBalance(BalanceType.AVAILABLE);
            if (totalSpend.compareTo(availableBalance) <= 0) {
                // there is enough money
            } else {
                // not enough funds - check if estimated balance suggests there
                // is (different error message)
                if (totalSpend.compareTo(controller.getModel().getWallet().getBalance(BalanceType.ESTIMATED)) <= 0) {
                    // the UI shows enough money but there is not enough
                    // available
                    notEnoughFundsButEstimatedOk = Boolean.TRUE;
                } else {
                    // the total spend is less than both the estimated and
                    // available - not confusing
                    notEnoughFunds = Boolean.TRUE;
                }
            }
        }
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, amountIsInvalid.toString());
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, notEnoughFunds.toString());
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS_BUT_ESTIMATED_OK,
                notEnoughFundsButEstimatedOk.toString());

        return !amountIsInvalid.booleanValue() && !notEnoughFunds.booleanValue()
                && !notEnoughFundsButEstimatedOk.booleanValue();
    }

    private boolean validateAddress(String address) {
        // copy address to user preferences
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_ADDRESS_VALUE, address);

        Boolean addressIsInvalid = Boolean.TRUE;
        try {
            Address parsedAddress = new Address(controller.getMultiBitService().getNetworkParameters(), address);
            addressIsInvalid = Boolean.FALSE;
        } catch (AddressFormatException afe) {
            // carry on
        }
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, addressIsInvalid.toString());

        return !addressIsInvalid.booleanValue();
    }

    public void clearValidationState() {
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_AMOUNT_IS_INVALID, Boolean.FALSE.toString());
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_NOT_ENOUGH_FUNDS, Boolean.FALSE.toString());
        controller.getModel().setUserPreference(MultiBitModel.VALIDATION_ADDRESS_IS_INVALID, Boolean.FALSE.toString());
    }
}
