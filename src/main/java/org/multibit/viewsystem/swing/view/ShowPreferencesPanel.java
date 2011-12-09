package org.multibit.viewsystem.swing.view;

import org.multibit.Localiser;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.action.ShowPreferencesSubmitAction;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * The show preferences view
 */
public class ShowPreferencesPanel extends JPanel implements View, DataProvider {

    private static final long serialVersionUID = 191352298245057705L;

    private MultiBitController controller;

    SortedSet<LanguageData> languageDataSet;

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

        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0),
                BorderFactory.createMatteBorder(1, 0, 1, 0, MultiBitFrame.DARK_BACKGROUND_COLOR.darker())));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        this.controller = controller;

        data = new Data();

        initUI();
    }

    /**
     * show preferences panel
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
        titleLabel.setText(controller.getLocaliser().getString("showPreferencesPanel.title"));
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

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 4;
        constraints.gridwidth = 1;
        constraints.weightx = 0.4;
        constraints.weighty = 0.06;
        constraints.anchor = GridBagConstraints.LINE_START;
        add(createButtonPanel(), constraints);

        JLabel filler1 = new JLabel();
        filler1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 5;
        constraints.gridwidth = 2;
        constraints.weightx = 1;
        constraints.weighty = 20;
        constraints.anchor = GridBagConstraints.NORTHWEST;
        add(filler1, constraints);
    }

    private JPanel createLanguagePanel() {
        // language radios
        JPanel languagePanel = new JPanel(new GridBagLayout());
        languagePanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0),
                BorderFactory.createTitledBorder(controller.getLocaliser().getString("showPreferencesPanel.languageTitle"))));
        languagePanel.setOpaque(false);

        GridBagConstraints constraints = new GridBagConstraints();

        ButtonGroup languageUsageGroup = new ButtonGroup();
        useDefaultLocale = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useDefault"));
        useDefaultLocale.setOpaque(false);
        
        JRadioButton useSpecific = new JRadioButton(controller.getLocaliser().getString("showPreferencesPanel.useSpecific"));
        useSpecific.setOpaque(false);
        
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

        // languages are added to the combo box in alphabetic order
        languageDataSet = new TreeSet<LanguageData>();

        for (int i = 0; i < numberOfLanguages; i++) {
            String languageCode = controller.getLocaliser().getString("showPreferencesPanel.languageCode." + (i + 1));
            String language = controller.getLocaliser().getString("showPreferencesPanel.language." + (i + 1));

            LanguageData languageData = new LanguageData();
            languageData.languageCode = languageCode;
            languageData.language = language;
            languageData.image = createImageIcon(languageCode);
            languageData.image.setDescription(language);
            languageDataSet.add(languageData);
        }

        Integer[] indexArray = new Integer[languageDataSet.size()];
        int index = 0;
        for (@SuppressWarnings("unused") LanguageData languageData : languageDataSet) {
            indexArray[index] = index;
            index++;
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
            int startingIndex = 0;
            Integer languageCodeIndex = 0;
            for (LanguageData languageData : languageDataSet) {
                if (languageData.languageCode.equals(userLanguageCode)) {
                    languageCodeIndex = startingIndex;
                    break;
                }
                startingIndex++;
            }
            if (languageCodeIndex != 0) {
                languageComboBox.setSelectedItem(languageCodeIndex.intValue());
                languageComboBox.setEnabled(true);
            }
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
        feePanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 2, 0, 0),
                BorderFactory.createTitledBorder(controller.getLocaliser().getString("showPreferencesPanel.feeTitle"))));
        feePanel.setOpaque(false);

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
        buttonPanel.setOpaque(false);
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

        BufferedImage bimg = new BufferedImage(26, 20, BufferedImage.TYPE_INT_RGB);
        Graphics2D g2 = bimg.createGraphics();

        g2.setColor(Color.WHITE);
        g2.setFont(font);
        g2.drawString(text, 3, 16);

        return new ImageIcon(bimg);
    }

    public Data getData() {
        Item languageItem = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
        if (useDefaultLocale.isSelected()) {
            languageItem.setNewValue(MultiBitModel.USER_LANGUAGE_IS_DEFAULT);
        } else {
            Integer selectedLanguageIndex = (Integer) languageComboBox.getSelectedItem();
            if (selectedLanguageIndex != null) {
                int loopIndex = 0;
                for (LanguageData languageData : languageDataSet) {
                    if (selectedLanguageIndex.intValue() == loopIndex) {
                        String newLanguageCode = languageData.languageCode;
                        languageItem.setNewValue(newLanguageCode);
                        break;
                    }
                    loopIndex++;
                }
            }
        }

        Item feeItem = new Item(MultiBitModel.SEND_FEE);
        feeItem.setOriginalValue(originalFee);
        feeItem.setNewValue(feeTextField.getText());
        data.addItem(MultiBitModel.SEND_FEE, feeItem);
        return data;
    }

    public JPanel getFormPanel() {
        return null;
    }

    public JTextField getLabelTextField() {
        return null;
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
                selectedIndex = (Integer) value;
            }
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text. If icon was null, say so.
            int loopIndex = 0;
            for (LanguageData languageData : languageDataSet) {
                if (selectedIndex == loopIndex) {
                    ImageIcon icon = languageData.image;
                    String language =languageData.language;
                    setIcon(icon);
                    setText(language);
                    break;
                }
                loopIndex++;
            }

            setFont(list.getFont());

            return this;
        }
    }

    class LanguageData implements Comparable<LanguageData> {
        public String languageCode;
        public String language;
        public ImageIcon image;

        @Override
        public int compareTo(LanguageData other) {
            return languageCode.compareTo(other.languageCode);
        }
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}