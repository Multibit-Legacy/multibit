/**
 * 
 */
package org.multibit.viewsystem.swing.action;

import java.awt.event.ActionEvent;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.swing.view.panels.WalletInfoPanel;

/**
 * @author "Abhijith Reddy"
 * 
 */
public class SaveWalletInfoAction extends MultiBitSubmitAction {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4446059504140666680L;

	private WalletInfoPanel walletInfoPanel;
	
	public SaveWalletInfoAction(BitcoinController controller,WalletInfoPanel walletInfoPanel) {
		super(controller, "walletInfoAction.save", null, null, null);
		this.walletInfoPanel = walletInfoPanel;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent arg0) {
		this.bitcoinController.getModel().getActivePerWalletModelData().setWalletDescription( walletInfoPanel.getWalletDescription() );
		this.bitcoinController.onWalletChanged( bitcoinController.getModel().getActiveWallet());
	}

}
