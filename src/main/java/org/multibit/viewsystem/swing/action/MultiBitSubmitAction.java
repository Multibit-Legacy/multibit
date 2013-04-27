package org.multibit.viewsystem.swing.action;

import javax.swing.AbstractAction;
import javax.swing.Icon;

import org.multibit.controller.Controller;
import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.PerWalletModelData;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * Abstract super class to check for whether wallet files have changed and
 * whether there is an active wallet available
 * @author jim
 *
 */
public abstract class MultiBitSubmitAction extends AbstractAction {
    private static final long serialVersionUID = 3750799470657961967L;

    protected final Controller controller;
    protected final MultiBitController multiBitController;
    
    /**
     * Creates a new {@link ResetTransactionsSubmitAction}.
     */
    public MultiBitSubmitAction(MultiBitController multiBitController, String textKey, String tooltipKey, String mnemonicKey,  Icon icon) {
        super(multiBitController.getLocaliser().getString(textKey), icon);
        this.multiBitController = multiBitController;
        this.controller = this.multiBitController;
        
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
        if (controller.getModel().thereIsNoActiveWallet()) {
            MessageManager.INSTANCE.addMessage(new Message(controller.getLocaliser().getString("multiBitSubmitAction.thereIsNoActiveWallet")));
            return true;
        }

        // check to see if another process has changed the active wallet
        PerWalletModelData perWalletModelData = controller.getModel().getActivePerWalletModelData();
        boolean haveFilesChanged = this.multiBitController.getFileHandler().haveFilesChanged(perWalletModelData);
        
        if (haveFilesChanged) {
            // set on the perWalletModelData that files have changed and fire
            // data changed
            perWalletModelData.setFilesHaveBeenChangedByAnotherProcess(true);
            this.multiBitController.fireFilesHaveBeenChangedByAnotherProcess(perWalletModelData); 
 
            return true;
        }
        
        return false;
    }
}
