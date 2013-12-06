/**
 * 
 */
package org.multibit.viewsystem.swing.view.panels;

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.SaveWalletInfoAction;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.multibit.viewsystem.swing.view.components.MultiBitTextField;
import org.multibit.viewsystem.swing.view.components.MultiBitTitledPanel;

/**
 * @author "Abhijith Reddy"
 * 
 */
public class WalletInfoPanel extends JPanel implements Viewable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7232817583153558772L;
	private static final int TEXTFIELD_VERTICAL_DELTA = 16;
	private JTextField descriptionText;
	private BitcoinController bitcoinController;
	private MultiBitButton saveWalletDescriptionButton;
	
	public WalletInfoPanel(BitcoinController controller) {
		applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
		bitcoinController = controller;
		initUI();
	}

	private void initUI() {
		setLayout(new GridBagLayout());

		JPanel descriptionPanel = new JPanel();
		descriptionPanel.setLayout(new GridBagLayout());

		JLabel label = new JLabel(bitcoinController.getLocaliser().getString("walletInfoAction.description"));
		label.setMinimumSize(new Dimension(60, 60));
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 2;
		constraints.gridy = 1;
		constraints.weightx = 1;
		constraints.weighty = 0.2;
		constraints.gridwidth = 1;
		constraints.gridheight = 1;
		constraints.anchor = GridBagConstraints.LINE_END;
		descriptionPanel.add(label, constraints);
		MultiBitLabel notUsedWalletDescriptionLabel = new MultiBitLabel(bitcoinController.getLocaliser().getString("walletInfoAction.description"));
		descriptionPanel.add(MultiBitTitledPanel.createStent((int)notUsedWalletDescriptionLabel.getPreferredSize().getWidth()), constraints);
		
		descriptionText = new MultiBitTextField("", 24, null);
		descriptionText.setHorizontalAlignment(JTextField.LEADING);
		Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        FontMetrics fontMetrics = this.getFontMetrics(font);
		int longFieldWidth = fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT);
		descriptionText.setHorizontalAlignment(JTextField.TRAILING);
		descriptionText.setMinimumSize(new Dimension((int) (longFieldWidth), getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
				+ TEXTFIELD_VERTICAL_DELTA));
		descriptionText.setPreferredSize(new Dimension((int) (longFieldWidth), getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
				+ TEXTFIELD_VERTICAL_DELTA));
		descriptionText.setMaximumSize(new Dimension((int) (longFieldWidth), getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight()
				+ TEXTFIELD_VERTICAL_DELTA));
		
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 4;
		constraints.gridy = 1;
		constraints.weightx = 1;
		constraints.weighty = 0.2;
		constraints.gridwidth = 1;
		constraints.gridheight = 1;
		constraints.anchor = GridBagConstraints.LINE_END;
		descriptionPanel.add(descriptionText, constraints);

		SaveWalletInfoAction saveWalletAction = new SaveWalletInfoAction(bitcoinController,this);
		saveWalletDescriptionButton = new MultiBitButton(saveWalletAction,bitcoinController);
		 constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.weighty = 1.0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
		descriptionPanel.add(saveWalletDescriptionButton, constraints);
		
		add(descriptionPanel);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.multibit.viewsystem.Viewable#displayView(org.multibit.viewsystem.
	 * DisplayHint)
	 */
	@Override
	public void displayView(DisplayHint displayHint) {
		descriptionText.setText(bitcoinController.getModel().getActiveWallet().getDescription());
		descriptionText.setCaretPosition(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.multibit.viewsystem.Viewable#getViewIcon()
	 */
	@Override
	public Icon getViewIcon() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.multibit.viewsystem.Viewable#getViewId()
	 */
	@Override
	public View getViewId() {
		return View.WALLET_INFO_VIEW;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.multibit.viewsystem.Viewable#getViewTitle()
	 */
	@Override
	public String getViewTitle() {
		 return bitcoinController.getLocaliser().getString("walletInfoAction.text");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.multibit.viewsystem.Viewable#getViewTooltip()
	 */
	@Override
	public String getViewTooltip() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.multibit.viewsystem.Viewable#navigateAwayFromView()
	 */
	@Override
	public void navigateAwayFromView() {
		// TODO Auto-generated method stub

	}

	
	public String getWalletDescription(){
		return descriptionText.getText();
	}
}
