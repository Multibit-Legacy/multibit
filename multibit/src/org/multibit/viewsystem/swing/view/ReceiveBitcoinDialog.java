package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.lang.reflect.Method;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.multibit.MultiBitController;
import org.multibit.viewsystem.Localiser;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CopyAddressAction;
import org.multibit.viewsystem.swing.action.CreateNewReceivingAddressAction;
import org.multibit.viewsystem.swing.action.OkBackToPreviousAction;
import org.multibit.viewsystem.swing.action.OpenAddressBookReceivingAction;

public class ReceiveBitcoinDialog extends JDialog {

    private static final long serialVersionUID = -2065108865497842662L;

    private static final String RECEIVE_BITCOIN_BIG_ICON_FILE = "/images/receive-big.jpg";

    private JFrame mainFrame;

    private MultiBitController controller;
    private Localiser localiser;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private JButton copyAddressButton;

    public ReceiveBitcoinDialog(JFrame mainFrame, MultiBitController controller, Localiser localiser) {
        super();
        this.mainFrame = mainFrame;
        this.controller = controller;
        this.localiser = localiser;

        initUI();

        pack();
        copyAddressButton.requestFocusInWindow();

    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.25D, 0.25D);
        setMinimumSize(new Dimension(550, 220));
        setTitle(localiser.getString("receiveBitcoinDialog.title"));
        setLayout(new BorderLayout());
        add(createReceiveBitcoinsPanel(), BorderLayout.CENTER);
        add(createButtonPanel(), BorderLayout.SOUTH);
    }

    private JPanel createReceiveBitcoinsPanel() {       
        JPanel receiveBitcoinsPanel = new JPanel();
        
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        buttonPanel.setLayout(flowLayout);
        
        receiveBitcoinsPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        JPanel filler1 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler1, constraints);

        ImageIcon receiveBigIcon = createImageIcon(RECEIVE_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(new JLabel(receiveBigIcon), constraints);
        
        JLabel helpLabel1 = new JLabel(
                localiser.getString("receiveBitcoinDialog.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 2;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(helpLabel1, constraints);

        JPanel filler2 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 3;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler2, constraints);
      
        JPanel filler3 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler3, constraints);

        JLabel addressLabel = new JLabel(localiser.getString("receiveBitcoinDialog.addressLabel"));
        addressLabel.setToolTipText(localiser
                .getString("receiveBitcoinDialog.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(addressLabel, constraints);

        addressTextField = new JTextField();
        addressTextField.setHorizontalAlignment(JTextField.LEFT);

        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressTextField, constraints);

        OpenAddressBookReceivingAction openAddressBookReceivingAction = new OpenAddressBookReceivingAction(controller, localiser);
        JButton addressBookButton = new JButton(openAddressBookReceivingAction);
//        addressBookButton.setText(localiser.getString("receiveBitcoinDialog.addressBookButton"));
//        addressBookButton.setToolTipText(localiser.getString("receiveBitcoinDialog.addressBookButton.tooltip"));
//        addressBookButton.addActionListener(new AddressBookButtonListener());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 3;
        constraints.weightx = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(addressBookButton, constraints);

        JLabel labelLabel = new JLabel(localiser.getString("receiveBitcoinDialog.labelLabel"));
        labelLabel.setToolTipText(localiser.getString("receiveBitcoinDialog.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        receiveBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField();
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        constraints.gridx = 2;
        constraints.gridy = 4;
        constraints.weightx = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(labelTextField, constraints);

        JLabel filler4 = new JLabel("");
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.2;
        constraints.anchor = GridBagConstraints.LINE_START;
        receiveBitcoinsPanel.add(filler4, constraints);
      
        return receiveBitcoinsPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        CopyAddressAction copyAddressAction = new CopyAddressAction(controller, localiser);
        copyAddressButton = new JButton(copyAddressAction);
//        copyAddressButton.setText(localiser.getString("receiveBitcoinDialog.copyAddressButton"));
//        copyAddressButton.setToolTipText(localiser.getString("receiveBitcoinDialog.copyAddressButton.tooltip"));
//        copyAddressButton.addActionListener(new CopyAddressButtonListener());
        buttonPanel.add(copyAddressButton);

        CreateNewReceivingAddressAction createNewReceivingAddressAction = new CreateNewReceivingAddressAction(controller, localiser);
        JButton createNewButton = new JButton(createNewReceivingAddressAction);
//        createNewButton.setText(localiser.getString("receiveBitcoinDialog.createNewButton"));
//        createNewButton.setToolTipText(localiser.getString("receiveBitcoinDialog.createNewButton.tooltip"));
//        createNewButton.addActionListener(new CreateNewButtonListener());
        buttonPanel.add(createNewButton);

        OkBackToPreviousAction okBackToPreviousAction = new OkBackToPreviousAction(controller, localiser);
        JButton okButton = new JButton(okBackToPreviousAction);
//        okButton.setText(localiser.getString("receiveBitcoinDialog.okButton"));
//        okButton.setToolTipText(localiser.getString("receiveBitcoinDialog.okButton.tooltip"));
//        okButton.addActionListener(new OkButtonListener());
        buttonPanel.add(okButton);

        return buttonPanel;
    }

    /**
     * Positions the specified dialog at a position relative to its parent.
     * 
     * @param dialog
     *            the dialog to be positioned.
     * @param horizontalPercent
     *            the relative location.
     * @param verticalPercent
     *            the relative location.
     */
    private void positionDialogRelativeToParent(final JDialog dialog,
            final double horizontalPercent, final double verticalPercent) {
        final Dimension d = dialog.getSize();
        final Dimension p = mainFrame.getSize();

        final int baseX = mainFrame.getX() - d.width;
        final int baseY = mainFrame.getY() - d.height;
        final int w = d.width + p.width;
        final int h = d.height + p.height;
        int x = baseX + (int) (horizontalPercent * w);
        int y = baseY + (int) (verticalPercent * h);

        // make sure the dialog fits completely on the screen...
        final Rectangle s = getMaximumWindowBounds();
        x = Math.min(x, (s.width - d.width));
        x = Math.max(x, 0);
        y = Math.min(y, (s.height - d.height));
        y = Math.max(y, 0);

        dialog.setBounds(x + s.x, y + s.y, d.width, d.height);

    }

    /**
     * Computes the maximum bounds of the current screen device. If this method
     * is called on JDK 1.4, Xinerama-aware results are returned. (See
     * Sun-Bug-ID 4463949 for details).
     * 
     * @return the maximum bounds of the current screen.
     */
    private Rectangle getMaximumWindowBounds() {
        final GraphicsEnvironment localGraphicsEnvironment = GraphicsEnvironment
                .getLocalGraphicsEnvironment();
        try {
            final Method method = GraphicsEnvironment.class.getMethod("getMaximumWindowBounds",
                    (Class[]) null);
            return (Rectangle) method.invoke(localGraphicsEnvironment, (Object[]) null);
        } catch (Exception e) {
            // ignore ... will fail if this is not a JDK 1.4 ..
        }

        final Dimension s = Toolkit.getDefaultToolkit().getScreenSize();
        return new Rectangle(0, 0, s.width, s.height);
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err
                    .println("com.google.bitcoin.tools.viewer.ViewerFrame#createImageIcon: Could not find file: "
                            + path);
            return null;
        }
    }
    
//    private class OkButtonListener implements ActionListener {
//        OkButtonListener() {
//        }
//
//        public void actionPerformed(ActionEvent e) {
//            thisDialog.setVisible(false);
//        }
//    }
    
//    private class CopyAddressButtonListener implements ActionListener {
//        CopyAddressButtonListener() {
//        }
//
//        public void actionPerformed(ActionEvent e) {
//            JOptionPane.showMessageDialog(thisDialog, "TODO - Copy Address");
//        }
//    }
      
//    private class CreateNewButtonListener implements ActionListener {
//        CreateNewButtonListener() {
//        }
//
//        public void actionPerformed(ActionEvent e) {
//            CreateEditAddressDialog createAddressDialog = new CreateEditAddressDialog(mainFrame, localiser, 
//                    "receiveBitcoinDialog.createAddressDialog.title",
//                    "receiveBitcoinDialog.createAddressDialog.helpTextKey1",
//                    "receiveBitcoinDialog.createAddressDialog.helpTextKey2");
//            createAddressDialog.setVisible(true);
//        }
//    }
       
//    private class AddressBookButtonListener implements ActionListener {
//        AddressBookButtonListener() {
//        }
//
//        public void actionPerformed(ActionEvent e) {
//            AddressBookDialog addressBookDialog = new AddressBookDialog(localiser, mainFrame, true);
//            addressBookDialog.setVisible(true);
//       }
//    }
}