package org.multibit.viewsystem.swing.view;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ResetTransactionsSubmitAction;

/**
 * The reset blockchain and transactions view
 */
public class ResetTransactionsPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 199992298245057705L;

    private MultiBitController controller;

    private Data data;

    private JLabel walletFilenameLabel;

    private JLabel walletDescriptionLabel;

    /**
     * Creates a new {@link ResetTransactionsPanel}.
     */
    public ResetTransactionsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        initUI();
    }

    /**
     * show explanatory text for resetting blockchain and transactions and a
     * button to do it
     */
    public void displayView() {
        walletFilenameLabel.setText(controller.getModel().getActiveWalletFilename());
        walletDescriptionLabel.setText(controller.getModel().getActivePerWalletModelData().getWalletDescription());
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    private void initUI() {
        setMinimumSize(new Dimension(550, 160));

        GridBagConstraints constraints = new GridBagConstraints();
        setLayout(new GridBagLayout());

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        JPanel fillerPanel1 = new JPanel();
        fillerPanel1.setOpaque(false);
        add(fillerPanel1, constraints);

        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.LEFT);
        titleLabel.setText(controller.getLocaliser().getString("resetTransactionsPanel.title"));
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        add(createExplainPanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.CENTER;
        add(createButtonPanel(), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.CENTER;
        add(filler1, constraints);
    }

    private JPanel createExplainPanel() {
        JPanel explainPanel = new JPanel(new GridBagLayout());
        explainPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0),
                BorderFactory.createTitledBorder(controller.getLocaliser().getString("resetTransactionsPanel.explainTitle"))));
        explainPanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel explainLabel1 = new JLabel(controller.getLocaliser().getString("resetTransactionsPanel.explainLabel.text1"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(explainLabel1, constraints);

        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(filler1, constraints);

        JPanel filler2 = new JPanel();
        filler2.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.05;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        explainPanel.add(filler2, constraints);

        JLabel walletFilenameLabelLabel = new JLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletFilenameLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        explainPanel.add(walletFilenameLabelLabel, constraints);

        walletFilenameLabel = new JLabel(controller.getModel().getActiveWalletFilename());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 2;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(walletFilenameLabel, constraints);

        JLabel walletDescriptionLabelLabel = new JLabel(controller.getLocaliser().getString(
                "resetTransactionsPanel.walletDescriptionLabel"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        explainPanel.add(walletDescriptionLabelLabel, constraints);

        walletDescriptionLabel = new JLabel(controller.getModel().getActivePerWalletModelData().getWalletDescription());
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 3;
        constraints.weightx = 0.5;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(walletDescriptionLabel, constraints);

        JPanel filler3 = new JPanel();
        filler3.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(filler3, constraints);

 
        JLabel explainLabel2 = new JLabel(controller.getLocaliser().getString("resetTransactionsPanel.explainLabel.text2"));
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        explainPanel.add(explainLabel2, constraints);

        return explainPanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setOpaque(false);
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        ResetTransactionsSubmitAction submitAction = new ResetTransactionsSubmitAction(controller, this);
        JButton submitButton = new JButton(submitAction);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    public Data getData() {
        return data;
    }

    public JPanel getFormPanel() {
        return null;
    }

    public JTextField getLabelTextField() {
        return null;
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub

    }
}