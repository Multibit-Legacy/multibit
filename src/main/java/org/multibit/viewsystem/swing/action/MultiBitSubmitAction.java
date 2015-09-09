package org.multibit.viewsystem.swing.action;

import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

import javax.swing.*;

/**
 * Abstract super class to check for whether wallet files have changed and
 * whether there is an active wallet available
 * @author jim
 *
 */
public abstract class MultiBitSubmitAction extends AbstractAction {
    private static final long serialVersionUID = 3750799470657961967L;

    protected final Controller controller;
    protected final BitcoinController bitcoinController;
    
    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public MultiBitSubmitAction(BitcoinController bitcoinController, String textKey, String tooltipKey, String mnemonicKey,  Icon icon) {
        super(bitcoinController.getLocaliser().getString(textKey), icon);
        this.bitcoinController = bitcoinController;
        this.controller = this.bitcoinController;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString(tooltipKey)));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic(mnemonicKey));
    }
   
    /**
     * Abort due to there not being an active wallet or the wallet has been changed by another process.
     * @return abort True if called method should abort
     */
    public boolean abort() {
        // Check if there is an active wallet.
        if (this.bitcoinController.getModel().thereIsNoActiveWallet()) {
            MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitSubmitAction.thereIsNoActiveWallet")));
            return true;
        }

        return false;
    }
}
