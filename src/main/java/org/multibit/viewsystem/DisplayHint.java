package org.multibit.viewsystem;

/**
 * A hint as to what caused the change in display.
 * This is used to minimise unnecessary redrawing.
 * 
 * @author jim
 *
 */
public enum DisplayHint {
    COMPLETE_REDRAW,
    WALLET_TRANSACTIONS_HAVE_CHANGED
}
