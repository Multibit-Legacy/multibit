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

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.OpenAddressBookAction;
import org.multibit.viewsystem.swing.action.PasteAddressAction;
import org.multibit.viewsystem.swing.action.SendBitcoinConfirmAction;

public class SendBitcoinDialog extends JDialog {

    private static final long serialVersionUID = -2065108657497842662L;

    private static final String SEND_BITCOIN_BIG_ICON_FILE = "/images/send-big.jpg";

    private JFrame mainFrame;

    private MultiBitController controller;
    private Localiser localiser;

    private JTextField addressTextField;

    private JTextField labelTextField;

    private JTextField amountTextField;

    public SendBitcoinDialog(JFrame mainFrame, MultiBitController controller, Localiser localiser) {
        super();
        this.mainFrame = mainFrame;
        this.controller = controller;
        this.localiser = localiser;

        initUI();

        pack();
        addressTextField.requestFocusInWindow();

    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.16D, 0.25D);
        setMinimumSize(new Dimension(740, 240));
        setTitle(localiser.getString("sendBitcoinDialog.title"));
        setLayout(new BorderLayout());
        add(createSendBitcoinsPanel(), BorderLayout.CENTER);
        add(createButtonsPanel(), BorderLayout.SOUTH);
    }

    private JPanel createSendBitcoinsPanel() {
        JPanel sendBitcoinsPanel = new JPanel();
        sendBitcoinsPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JPanel filler0 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.05;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler0, constraints);

        ImageIcon sendBigIcon = createImageIcon(SEND_BITCOIN_BIG_ICON_FILE);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.10;
        constraints.gridwidth = 1;
        constraints.gridheight = 2;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(new JLabel(sendBigIcon), constraints);

        JLabel helpLabel1 = new JLabel(
                localiser.getString("sendBitcoinDialog.helpLabel1.message"));
        helpLabel1.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.04;
        constraints.gridwidth = 2;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(helpLabel1, constraints);
      
        JLabel helpLabel2 = new JLabel(
                localiser.getString("sendBitcoinDialog.helpLabel2.message"));
        helpLabel2.setHorizontalAlignment(JLabel.LEFT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.3;
        constraints.weighty = 0.04;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(helpLabel2, constraints);
      
        JPanel filler1 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 3;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler1, constraints);
      
        JLabel addressLabel = new JLabel(localiser.getString("sendBitcoinDialog.addressLabel"));
        addressLabel.setToolTipText(localiser.getString("sendBitcoinDialog.addressLabel.tooltip"));
        addressLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(addressLabel, constraints);

        addressTextField = new JTextField();
        addressTextField.setHorizontalAlignment(JTextField.LEFT);

        constraints.gridx = 1;
        constraints.gridy = 4;
        constraints.weightx = 0.8;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(addressTextField, constraints);

        PasteAddressAction pasteAddressAction = new PasteAddressAction(controller);
        JButton pasteAddressButton = new JButton(pasteAddressAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 3;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(pasteAddressButton, constraints);

        OpenAddressBookAction openAddressBookSendingAction = new OpenAddressBookAction(controller, true, false);
        JButton addressBookButton = new JButton(openAddressBookSendingAction);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 4;
        constraints.gridy = 4;
        constraints.weightx = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(addressBookButton, constraints);

        JLabel labelLabel = new JLabel(localiser.getString("sendBitcoinDialog.labelLabel"));
        labelLabel.setToolTipText(localiser.getString("sendBitcoinDialog.labelLabel.tooltip"));
        labelLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(labelLabel, constraints);

        labelTextField = new JTextField();
        labelTextField.setHorizontalAlignment(JTextField.LEFT);
        constraints.gridx = 1;
        constraints.gridy = 5;
        constraints.weightx = 0.8;
        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(labelTextField, constraints);

        JPanel filler2 = new JPanel();
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 6;
        constraints.weightx = 0.3;
        constraints.weighty = 0.1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(filler2, constraints);
      
        JLabel amountLabel = new JLabel(localiser.getString("sendBitcoinDialog.amountLabel"));
        amountLabel.setToolTipText(localiser.getString("sendBitcoinDialog.amountLabel.tooltip"));
        amountLabel.setHorizontalAlignment(JLabel.RIGHT);
        constraints.gridx = 0;
        constraints.gridy = 7;
        constraints.gridwidth = 1;
        constraints.weightx = 0.5;
        constraints.weighty = 0.1;
        constraints.anchor = GridBagConstraints.LINE_END;
        sendBitcoinsPanel.add(amountLabel, constraints);

        amountTextField = new JTextField();
        amountTextField.setHorizontalAlignment(JTextField.RIGHT);
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 7;
        constraints.weightx = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(amountTextField, constraints);

        JLabel amountUnitLabel = new JLabel(
                localiser.getString("sendBitcoinDialog.amountUnitLabel"));
        amountUnitLabel.setToolTipText(localiser
                .getString("sendBitcoinDialog.amountUnitLabel.tooltip"));
        constraints.gridx = 2;
        constraints.gridy = 7;
        constraints.weightx = 0.4;
        constraints.anchor = GridBagConstraints.LINE_START;
        sendBitcoinsPanel.add(amountUnitLabel, constraints);

        return sendBitcoinsPanel;
    }

    private JPanel createButtonsPanel() {
        JPanel buttonsPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonsPanel.setLayout(flowLayout);

        CancelBackToParentAction cancelBackToParentAction = new CancelBackToParentAction(controller);
        JButton cancelButton = new JButton(cancelBackToParentAction);
        buttonsPanel.add(cancelButton);

        SendBitcoinConfirmAction sendBitcoinConfirmAction = new SendBitcoinConfirmAction(controller);
        JButton sendButton = new JButton(sendBitcoinConfirmAction);
        buttonsPanel.add(sendButton);

        return buttonsPanel;
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
}