package org.multibit.viewsystem.swing;

import java.awt.FlowLayout;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/*
 * JPanel used in status bar to present user with a combo box 
 * of units of Bitcoin to use to display Bitcoin amounts
 * 
 * Possible values:
 * 
 *  BTC     Bitcoin 
 *  mBTC    milliBitcoin
 *  uBTC    microBitcoin
 *  Satoshi 100,000,000 Satoshi = 1 BTC
 */
public class DisplayUsingPanel extends JPanel {
    JLabel displayUsingLabel;
 
    JComboBox unitComboBox;
    
    public DisplayUsingPanel () {
        super();
        setLayout(new FlowLayout());
        displayUsingLabel = new JLabel();
        String[] units = new String[] {"BTC", "mBTC", "uBTC", "Satoshi"};
        unitComboBox = new JComboBox(units);
        
        add(unitComboBox, FlowLayout.LEFT);
        add(displayUsingLabel, FlowLayout.LEFT);
        
        this.setToolTipText("1,000,000 microBitcoin (uBTC) = 1 Bitcoin");
    }

    public JLabel getDisplayUsingLabel() {
        return displayUsingLabel;
    }

    public JComboBox getUnitComboBox() {
        return unitComboBox;
    }

}
