package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;

import org.multibit.Localiser;
import org.multibit.action.Action;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ShowPreferencesSubmitAction;

/**
 * The help about view
 */
public class ShowPreferencesPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private Map<String, Integer> languageCodeToIndexMap;

    ImageIcon[] images;
    private Integer[] indexArray;
    private String[] languageCodeArray;
    private String[] languageArray;

    private JRadioButton useDefaultLocale;
    private JComboBox languageComboBox;

    private JTextField feeTextField;
    private String originalFee;

    private Data data;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public ShowPreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.GRAY));

        this.controller = controller;

        languageCodeToIndexMap = new HashMap<String, Integer>();

        data = new Data();

        initUI();
    }

    public String getDescription() {
        return controller.getLocaliser().getString("showPreferencesPanel.title");
    }

    /**
     * show help about message box
     */
    public void displayView() {
        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = Localiser.bitcoinValueToString4(MultiBitModel.SEND_MINIMUM_FEE, false, false);
        }
        originalFee = sendFeeString;
        feeTextField.setText(sendFeeString);
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
    }

    public void setPossibleActions(Collection<Action> possibleActions) {
        // not required in swing view
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
        add(fillerPanel1, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        JLabel titleLabel = new JLabel();
        titleLabel.setHorizontalTextPosition(JLabel.LEFT);
        titleLabel.setText(getDescription());
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE,
                MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        titleLabel.setFont(font);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 1;
        constraints.gridwidth = 1;
        constraints.weightx = 1.8;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(titleLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1.6;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(createLanguagePanel(), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(createFeePanel(), constraints);

        JLabel filler1 = new JLabel();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(filler1, constraints);
    }

    private JPanel createLanguagePanel() {
        // language radios
        JPanel languagePanel = new JPanel(new GridBagLayout());
        languagePanel.setBorder(BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showPreferencesPanel.languageTitle")));

        GridBagConstraints constraints = new GridBagConstraints();

        ButtonGroup languageUsageGroup = new ButtonGroup();
        useDefaultLocale = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useDefault"));
        JRadioButton useSpecific = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useSpecific"));
        ItemListener itemListener = new ChangeLanguageUsageItemListener();
        useDefaultLocale.addItemListener(itemListener);
        useSpecific.addItemListener(itemListener);
        languageUsageGroup.add(useDefaultLocale);
        languageUsageGroup.add(useSpecific);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(useDefaultLocale, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(useSpecific, constraints);

        // language combo box
        int numberOfLanguages = Integer.parseInt(controller.getLocaliser().getString("showPreferencesPanel.numberOfLanguages"));
        indexArray = new Integer[numberOfLanguages];
        images = new ImageIcon[numberOfLanguages];

        languageCodeArray = new String[numberOfLanguages];
        languageArray = new String[numberOfLanguages];
        for (int i = 0; i < numberOfLanguages; i++) {
            String languageCode = controller.getLocaliser().getString("showPreferencesPanel.languageCode." + (i + 1));
            String language = controller.getLocaliser().getString("showPreferencesPanel.language." + (i + 1));
            indexArray[i] = new Integer(i);
            languageCodeToIndexMap.put(languageCode, indexArray[i]);

            languageCodeArray[i] = languageCode;
            languageArray[i] = language;
            images[i] = createImageIcon(languageCode);
            if (images[i] != null) {
                images[i].setDescription(language);
            }
        }

        languageComboBox = new JComboBox(indexArray);
        ComboBoxRenderer renderer = new ComboBoxRenderer();
        renderer.setPreferredSize(new Dimension(150, 30));
        languageComboBox.setRenderer(renderer);

        // get the languageCode value stored in the model
        String userLanguageCode = controller.getModel().getUserPreference(MultiBitModel.USER_LANGUAGE_CODE);
        if (userLanguageCode == null || MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            useDefaultLocale.setSelected(true);
            languageComboBox.setEnabled(false);
        } else {
            useSpecific.setSelected(true);
            Integer languageCodeIndex = languageCodeToIndexMap.get(userLanguageCode);
            if (languageCodeIndex != 0)
            languageComboBox.setSelectedItem(languageCodeIndex.intValue());
            languageComboBox.setEnabled(true);
        }

        // store original value for use by submit action
        Item languageItem = new Item(MultiBitModel.USER_LANGUAGE_CODE);
        languageItem.setOriginalValue(userLanguageCode);
        data.addItem(MultiBitModel.USER_LANGUAGE_CODE, languageItem);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 2;
        constraints.weightx = 0.8;
        constraints.weighty = 0.6;
        constraints.anchor = GridBagConstraints.LINE_START;
        languagePanel.add(languageComboBox, constraints);

        // main panel layout
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 1;
        constraints.weighty = 0.15;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;

        return languagePanel;
    }

    private JPanel createFeePanel() {
        JPanel feePanel = new JPanel(new GridBagLayout());
        feePanel.setBorder(BorderFactory.createTitledBorder(controller.getLocaliser()
                .getString("showPreferencesPanel.feeTitle")));

        GridBagConstraints constraints = new GridBagConstraints();

        JLabel feeLabel = new JLabel(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.text"));
        feeLabel.setToolTipText(controller.getLocaliser().getString("showPreferencesPanel.feeLabel.tooltip"));
        JLabel feeCurrencyLabel = new JLabel("BTC");

        String sendFeeString = controller.getModel().getUserPreference(MultiBitModel.SEND_FEE);

        if (sendFeeString == null || sendFeeString == "") {
            sendFeeString = Localiser.bitcoinValueToString4(MultiBitModel.SEND_MINIMUM_FEE, false, false);
        }
        originalFee = sendFeeString;

        feeTextField = new JTextField(10);
        feeTextField.setHorizontalAlignment(JLabel.RIGHT);

        feeTextField.setText(sendFeeString);
        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        feePanel.add(feeLabel, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 0.3;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        feePanel.add(feeTextField, constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.weightx = 0.2;
        constraints.weighty = 0.3;
        constraints.anchor = GridBagConstraints.LINE_START;
        feePanel.add(feeCurrencyLabel, constraints);

        return feePanel;
    }

    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.RIGHT);
        buttonPanel.setLayout(flowLayout);

        ShowPreferencesSubmitAction submitAction = new ShowPreferencesSubmitAction(controller, this);
        JButton submitButton = new JButton(submitAction);
        buttonPanel.add(submitButton);

        return buttonPanel;
    }

    class ChangeLanguageUsageItemListener implements ItemListener {
        public ChangeLanguageUsageItemListener() {

        }

        public void itemStateChanged(ItemEvent e) {
            if (e.getSource().equals(useDefaultLocale)) {
                languageComboBox.setEnabled(false);
            } else {
                languageComboBox.setEnabled(true);
            }
        }
    }

    private ImageIcon createImageIcon(String text) {
        Font font = new Font("Dialog", Font.PLAIN, 16);

        BufferedImage bimg = new BufferedImage(25, 17, BufferedImage.TYPE_INT_RGB);
        Graphics2D g2 = bimg.createGraphics();

        g2.setColor(Color.WHITE);
        g2.setFont(font);
        g2.drawString(text, 3, 14);

        return new ImageIcon(bimg);
    }

    public Data getData() {
        Item languageItem = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
        if (useDefaultLocale.isSelected()) {
            languageItem.setNewValue(MultiBitModel.USER_LANGUAGE_IS_DEFAULT);
        } else {
            Integer selectedLanguageIndex = (Integer) languageComboBox.getSelectedItem();
            String newLanguageCode = languageCodeArray[selectedLanguageIndex.intValue()];
            languageItem.setNewValue(newLanguageCode);
        }

        Item feeItem = new Item(MultiBitModel.SEND_FEE);
        feeItem.setOriginalValue(originalFee);
        feeItem.setNewValue(feeTextField.getText());
        data.addItem(MultiBitModel.SEND_FEE, feeItem);
        return data;
    }

    class ComboBoxRenderer extends JLabel implements ListCellRenderer {
        private static final long serialVersionUID = -3301957214353702172L;

        public ComboBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }

        /*
         * This method finds the image and text corresponding to the selected
         * value and returns the label, set up to display the text and image.
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                boolean cellHasFocus) {
            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = 0;
            if (value != null) {
                selectedIndex = ((Integer) value).intValue();
            }
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text. If icon was null, say so.
            ImageIcon icon = images[selectedIndex];
            String language = languageArray[selectedIndex];
            setIcon(icon);
            setText(language);
            setFont(list.getFont());

            return this;
        }
    }

}