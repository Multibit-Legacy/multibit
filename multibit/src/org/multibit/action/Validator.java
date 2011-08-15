package org.multibit.action;

import org.multibit.controller.MultiBitController;

/**
 * a class to validate String addresses and amounts
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
     * @param address
     * @param amount
     * @return
     */
    public boolean validate(String address, String amount) {
        return true;
    }
}
