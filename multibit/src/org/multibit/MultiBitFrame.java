package org.multibit;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.google.bitcoin.core.Wallet;
import org.multibit.actions.ExitAction;
import org.multibit.actions.HelpAboutAction;
import org.multibit.actions.OpenWalletAction;
import org.multibit.actions.ReceiveBitcoinAction;
import org.multibit.actions.SaveWalletAsAction;
import org.multibit.actions.SendBitcoinAction;
import org.multibit.actions.ShowHelpContentsAction;
import org.multibit.actions.ViewAddressBookAction;
import org.multibit.actions.ViewPreferencesAction;
import org.multibit.watermark.FillPainter;
import org.multibit.watermark.WatermarkPainter;
import org.multibit.watermark.WatermarkViewport;

/*
 * JFrame displaying the contents of a Wallet
 */
public class MultiBitFrame extends JFrame {
    private static final String SAVE_AS_ICON_FILE = "/images/saveAs.jpg";
    private static final String OPEN_WALLET_ICON_FILE = "/images/openWallet.png";
    private static final String SEND_BITCOIN_ICON_FILE = "/images/send.jpg";
    private static final String RECEIVE_BITCOIN_ICON_FILE = "/images/receive.jpg";
    private static final String VIEW_ADDRESSBOOK_ICON_FILE = "/images/addressBook.jpg";
    private static final String PROGRESS_0_ICON_FILE = "/images/progress0.jpg";
    private static final String PROGRESS_2_ICON_FILE = "/images/progress2.jpg";
    private static final String PROGRESS_4_ICON_FILE = "/images/progress4.jpg";
    private static final String TICK_ICON_FILE = "/images/tick.jpg";
    private static final String HELP_CONTENTS_ICON_FILE = "/images/helpContents.jpg";
    private static final String BITCOINJ_ICON_FILE = "/images/bitcoinj.gif";
    private static final String TITLE_SEPARATOR = " - ";
    private static final double PROPORTION_OF_SCREEN_TO_FILL = 0.7D;

    private static final long serialVersionUID = 7621813615342923041L;

    private Wallet wallet;
    private String walletFilename;

    private Localiser localiser;

    private JTextField balanceTextField;

    private JLabel onlineStatusLabel, networkStatusLabel;

    private WalletTableModel tableModel;

    MultiBitFrame(String walletFilename, Localiser localiser) {
        this.localiser = localiser;
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        boolean walletNotLoaded = false;
        String walletNotLoadedErrorMessage = "";

        if (walletFilename != null) {
            File file = new File(walletFilename);

            try {
                wallet = Wallet.loadFromFile(file);
                setWallet(wallet, walletFilename);

            } catch (IOException e) {
                walletNotLoaded = true;
                walletNotLoadedErrorMessage = localiser.getString(
                        "multiBitFrame.walletNotLoaded",
                        new Object[] { walletFilename,
                                e.getClass().getName() + ": " + e.getMessage() });
            }
        } else {
            setTitle(localiser.getString("multiBitFrame.title"));
        }

        sizeAndCenter();

        initUI();

        String balanceText = "19 BTC";
        balanceTextField.setText(balanceText);

        pack();
        setVisible(true);

        onlineStatusLabel.setText("Online");
        networkStatusLabel.setText("Synchronising with network... (96% done)");

        if (walletNotLoaded) {
            JOptionPane.showMessageDialog(this, walletNotLoadedErrorMessage,
                    localiser.getString("multiBitFrame.walletNotLoadedMessageBoxTitle"),
                    JOptionPane.ERROR_MESSAGE, new ImageIcon(this.getIconImage()));
        }
    }

    /**
     * set a new wallet onto the frame
     * 
     * @param wallet
     * @param walletPathname
     */
    public void setWallet(Wallet wallet, String walletFilename) {
        this.wallet = wallet;
        this.walletFilename = walletFilename;

        this.setTitle(walletFilename + TITLE_SEPARATOR + localiser.getString("multiBitFrame.title"));

        if (tableModel != null) {
            tableModel.setWallet(wallet);
        }
        // TODO refresh display properly with listeners
    }

    /**
     * get the current wallet
     */
    public Wallet getWallet() {
        return wallet;
    }

    /**
     * get the current wallet filename
     */
    public String getWalletFilename() {
        return walletFilename;
    }

    private void sizeAndCenter() {
        // get the screen size as a java dimension
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        int height = (int) (screenSize.height * PROPORTION_OF_SCREEN_TO_FILL);
        int width = (int) (screenSize.width * PROPORTION_OF_SCREEN_TO_FILL);

        // set the jframe height and width
        setPreferredSize(new Dimension(width, height));
        double startPositionRatio = (1 - PROPORTION_OF_SCREEN_TO_FILL) / 2;
        setLocation((int) (width * startPositionRatio), (int) (height * startPositionRatio));

        // TODO remember screen size and position in config file
    }

    private void initUI() {
        Container contentPane = getContentPane();
        contentPane.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        // set the application icon
        ImageIcon imageIcon = createImageIcon(BITCOINJ_ICON_FILE);
        if (imageIcon != null) {
            setIconImage(imageIcon.getImage());
        }

        JToolBar toolBar = addMenuBarAndCreateToolBar(constraints, contentPane);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.85;
        constraints.weighty = 0.06;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(toolBar, constraints);

        JPanel balancePanel = createBalancePanel();
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.15;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;

        contentPane.add(balancePanel, constraints);

        JComponent walletPanel = createWalletPanel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.88;
        constraints.anchor = GridBagConstraints.LINE_START;
        contentPane.add(walletPanel, constraints);

        StatusBar statusBar = new StatusBar();
        onlineStatusLabel = new JLabel();
        onlineStatusLabel.setHorizontalAlignment(SwingConstants.CENTER);
        networkStatusLabel = new JLabel();
        statusBar.setZones(new String[] { "online", "network" }, new JComponent[] {
                onlineStatusLabel, networkStatusLabel }, new String[] { "8%", "*" });
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        contentPane.add(statusBar, constraints);
    }

    private JPanel createBalancePanel() {
        JPanel balancePanel = new JPanel();
        balancePanel.setMinimumSize(new Dimension(180, 30));
        balancePanel.setPreferredSize(new Dimension(180, 30));
        balancePanel.setOpaque(true);
        Border border = BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK),
                BorderFactory.createEmptyBorder(0, 3, 0, 3));
        balancePanel.setBorder(border);
        balancePanel.setBackground(Color.WHITE);

        balancePanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        JLabel balanceLabel = new JLabel(localiser.getString("multiBitFrame.balanceLabel"));

        balanceLabel.setFont(balanceLabel.getFont().deriveFont(java.awt.Font.BOLD));
        balanceLabel.setToolTipText(localiser.getString("multiBitFrame.balanceLabel.tooltip"));
        balanceLabel.setHorizontalAlignment(JLabel.RIGHT);

        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.anchor = GridBagConstraints.LINE_END;

        balancePanel.add(balanceLabel, constraints);

        balanceTextField = new JTextField();
        balanceTextField.setEditable(false);
        balanceTextField.setHorizontalAlignment(JTextField.RIGHT);
        balanceTextField.setBorder(BorderFactory.createEmptyBorder());
        balanceTextField.setBackground(Color.WHITE);
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.8;
        constraints.anchor = GridBagConstraints.LINE_END;
        balancePanel.add(balanceTextField, constraints);

        return balancePanel;
    }

    private JPanel createWalletPanel() {
        JPanel walletPanel = new JPanel();
        walletPanel.setOpaque(false);

        walletPanel.setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tableModel = new WalletTableModel(wallet, localiser);
        JTable table = new JTable(tableModel);
        table.setOpaque(false);
        table.setShowGrid(false);

        // use status icons
        table.getColumnModel().getColumn(0).setCellRenderer(new ImageRenderer());

        // date centered
        table.getColumnModel().getColumn(1).setCellRenderer(new CenterJustifiedRenderer());

        // center column headers
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);

        // description left justified
        table.getColumnModel().getColumn(2).setCellRenderer(new LeftJustifiedRenderer());

        // credit and debit right justified
        table.getColumnModel().getColumn(3).setCellRenderer(new RightJustifiedRenderer());
        table.getColumnModel().getColumn(4).setCellRenderer(new RightJustifiedRenderer());

        TableColumn tableColumn = table.getColumnModel().getColumn(0); // status
        tableColumn.setPreferredWidth(40);

        tableColumn = table.getColumnModel().getColumn(1); // date
        tableColumn.setPreferredWidth(80);

        tableColumn = table.getColumnModel().getColumn(2); // description
        tableColumn.setPreferredWidth(300);

        tableColumn = table.getColumnModel().getColumn(3); // debit
        tableColumn.setPreferredWidth(50);

        tableColumn = table.getColumnModel().getColumn(4); // credit
        tableColumn.setPreferredWidth(50);

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        WatermarkPainter bgPainter = new FillPainter();
        WatermarkViewport vp = new WatermarkViewport(bgPainter, null);
        vp.setView(table);
        scrollPane.setViewport(vp);

        walletPanel.add(scrollPane, constraints);

        return walletPanel;
    }

    private JToolBar addMenuBarAndCreateToolBar(GridBagConstraints constraints,
            Container contentPane) {
        // Create the menu bar
        JMenuBar menuBar = new JMenuBar();

        // Create the toolBar
        JToolBar toolBar = new JToolBar();
        toolBar.setFloatable(false);

        // Build the File menu.
        JMenu fileMenu = new JMenu(localiser.getString("multiBitFrame.fileMenuText"));
        fileMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.fileMenuMnemonic"));
        menuBar.add(fileMenu);

        // Build the Trade menu.
        JMenu tradeMenu = new JMenu(localiser.getString("multiBitFrame.tradeMenuText"));
        tradeMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.tradeMenuMnemonic"));
        menuBar.add(tradeMenu);

        // Build the View menu.
        JMenu viewMenu = new JMenu(localiser.getString("multiBitFrame.viewMenuText"));
        viewMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.viewMenuMnemonic"));
        menuBar.add(viewMenu);

        // Build the Help menu.
        JMenu helpMenu = new JMenu(localiser.getString("multiBitFrame.helpMenuText"));
        helpMenu.setMnemonic(localiser.getMnemonic("multiBitFrame.helpMenuMnemonic"));
        menuBar.add(helpMenu);

        // open wallet action
        OpenWalletAction openWalletAction = new OpenWalletAction(localiser,
                createImageIcon(OPEN_WALLET_ICON_FILE), this);
        JMenuItem menuItem = new JMenuItem(openWalletAction);
        fileMenu.add(menuItem);
        JButton openWalletButton = new JButton(openWalletAction);
        openWalletButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        openWalletButton.setHorizontalTextPosition(AbstractButton.CENTER);
        toolBar.add(openWalletButton);

        // save wallet as action
        SaveWalletAsAction saveWalletAsAction = new SaveWalletAsAction(localiser,
                createImageIcon(SAVE_AS_ICON_FILE), this);
        menuItem = new JMenuItem(saveWalletAsAction);
        fileMenu.add(menuItem);

        fileMenu.addSeparator();

        // exit action
        menuItem = new JMenuItem(new ExitAction(localiser));
        fileMenu.add(menuItem);

        // show help contents action
        ShowHelpContentsAction showHelpContentsAction = new ShowHelpContentsAction(localiser,
                createImageIcon(HELP_CONTENTS_ICON_FILE), this);
        menuItem = new JMenuItem(showHelpContentsAction);
        helpMenu.add(menuItem);

        // help about action
        HelpAboutAction helpAboutAction = new HelpAboutAction(localiser, this);
        menuItem = new JMenuItem(helpAboutAction);
        helpMenu.add(menuItem);

        toolBar.addSeparator();

        // receive bitcoin action
        ReceiveBitcoinAction receiveBitcoinAction = new ReceiveBitcoinAction(localiser,
                createImageIcon(RECEIVE_BITCOIN_ICON_FILE), this);
        tradeMenu.add(receiveBitcoinAction);
        JButton receiveBitcoinButton = new JButton(receiveBitcoinAction);
        receiveBitcoinButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        receiveBitcoinButton.setHorizontalTextPosition(AbstractButton.CENTER);
        toolBar.add(receiveBitcoinButton);

        // send bitcoin action
        SendBitcoinAction sendBitcoinAction = new SendBitcoinAction(localiser,
                createImageIcon(SEND_BITCOIN_ICON_FILE), this);
        menuItem = new JMenuItem(sendBitcoinAction);
        tradeMenu.add(menuItem);

        JButton sendBitcoinButton = new JButton(sendBitcoinAction);
        sendBitcoinButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        sendBitcoinButton.setHorizontalTextPosition(AbstractButton.CENTER);
        toolBar.add(sendBitcoinButton);

        toolBar.addSeparator();

        // view address book action
        ViewAddressBookAction viewAddressBookAction = new ViewAddressBookAction(localiser,
                createImageIcon(VIEW_ADDRESSBOOK_ICON_FILE), this);
        viewMenu.add(viewAddressBookAction);
        JButton viewAddressBookButton = new JButton(viewAddressBookAction);
        viewAddressBookButton.setVerticalTextPosition(AbstractButton.BOTTOM);
        viewAddressBookButton.setHorizontalTextPosition(AbstractButton.CENTER);
        toolBar.add(viewAddressBookButton);

        ViewPreferencesAction viewPreferencesAction = new ViewPreferencesAction(localiser, null,
                this);
        viewMenu.add(viewPreferencesAction);

        setJMenuBar(menuBar);

        return toolBar;
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err
                    .println("org.multibit.ViewerFrame#createImageIcon: Could not find file: "
                            + path);
            return null;
        }
    }

    class ImageRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 154545L;

        JLabel label = new JLabel();

        ImageIcon tickIcon = createImageIcon(TICK_ICON_FILE);
        ImageIcon progress0Icon = createImageIcon(PROGRESS_0_ICON_FILE);
        ImageIcon progress2Icon = createImageIcon(PROGRESS_2_ICON_FILE);
        ImageIcon progress4Icon = createImageIcon(PROGRESS_4_ICON_FILE);

        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus, int row, int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(false);

            switch (row) {
            case 0: {
                label.setIcon(progress0Icon);
                break;
            }
            case 1: {
                label.setIcon(progress2Icon);
                break;
            }
            case 2: {
                label.setIcon(progress4Icon);
                break;
            }

            default:
                label.setIcon(tickIcon);
            }
            return label;
        }
    }

    class RightJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus, int row, int column) {
            label.setHorizontalAlignment(SwingConstants.RIGHT);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }

    class LeftJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus, int row, int column) {
            label.setHorizontalAlignment(SwingConstants.LEFT);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }

    class CenterJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        JLabel label = new JLabel();

        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus, int row, int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setOpaque(false);

            label.setText((String) value);

            return label;
        }
    }
}