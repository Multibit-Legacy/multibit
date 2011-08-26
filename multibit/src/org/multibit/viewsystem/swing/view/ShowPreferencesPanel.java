package org.multibit.viewsystem.swing.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

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

    private ImageIcon imageIcon;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private Map<String, String> languageToLanguageCodeMap;
    private Map<String, String> languageCodeToLanguageMap;
    private JRadioButton useDefaultLocale;
    private JComboBox languageComboBox;

    private Data data;

    /**
     * Creates a new {@link ShowPreferencesPanel}.
     */
    public ShowPreferencesPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.LIGHT_GRAY));

        this.controller = controller;

        languageToLanguageCodeMap = new HashMap<String, String>();
        languageCodeToLanguageMap = new HashMap<String, String>();

        data = new Data();

        initUI();

        // String versionNumber = localiser.getVersionNumber();
        //
        // String versionText =
        // localiser.getString("helpAboutAction.versionText",
        // new Object[] { versionNumber });
        //
        // GridBagConstraints constraints = new GridBagConstraints();
        // setLayout(new GridBagLayout());
        //
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 0;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.12;
        // constraints.anchor = GridBagConstraints.CENTER;
        // JPanel fillerPanel1 = new JPanel();
        // add(fillerPanel1, constraints);
        //
        //
        // JLabel titleLabel = new JLabel();
        // titleLabel.setHorizontalTextPosition(JLabel.CENTER);
        // titleLabel.setText(getDescription());
        // Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME,
        // MultiBitFrame.MULTIBIT_FONT_STYLE,
        // MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
        // titleLabel.setFont(font);
        //
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 1;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.06;
        // constraints.anchor = GridBagConstraints.CENTER;
        // add(titleLabel, constraints);
        //
        // imageIcon = createImageIcon(SPLASH_ICON_FILE);
        // JLabel splashLabel = new JLabel();
        // splashLabel.setIcon(imageIcon);
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 2;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.64;
        // constraints.anchor = GridBagConstraints.CENTER;
        // add(splashLabel, constraints);
        //
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 3;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.06;
        // constraints.anchor = GridBagConstraints.CENTER;
        // JLabel urlLabel = new JLabel(MULTIBIT_URL);
        // add(urlLabel, constraints);
        //
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 4;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.06;
        // constraints.anchor = GridBagConstraints.CENTER;
        // JLabel versionLabel = new JLabel(versionText);
        // add(versionLabel, constraints);
        //
        // constraints.fill = GridBagConstraints.NONE;
        // constraints.gridx = 0;
        // constraints.gridy = 5;
        // constraints.gridwidth = 1;
        // constraints.weightx = 1;
        // constraints.weighty = 0.12;
        // constraints.anchor = GridBagConstraints.CENTER;
        // JPanel fillerPanel2 = new JPanel();
        // add(fillerPanel2, constraints);

    }

    public String getDescription() {
        return controller.getLocaliser().getString("showPreferencesDialog.title");
    }

    /**
     * show help about message box
     */
    public void displayView() {

    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
        // not implemented on this view
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    private ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = MultiBitFrame.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("org.multibit.ViewerFrame#createImageIcon: Could not find file: " + path);
            return null;
        }
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
        Font font = new Font(MultiBitFrame.MULTIBIT_FONT_NAME, MultiBitFrame.MULTIBIT_FONT_STYLE, MultiBitFrame.MULTIBIT_LARGE_FONT_SIZE + 2);
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
        add(createPreferencesPanel(), constraints);
     }

    private JPanel createPreferencesPanel() {
        JPanel preferencesPanel = new JPanel();
        preferencesPanel.setLayout(new GridBagLayout());

        // language radios
        JPanel languagePanel = new JPanel(new GridBagLayout());
        languagePanel.setBorder(BorderFactory.createTitledBorder(controller.getLocaliser().getString(
                "showPreferencesDialog.languageTitle")));

        GridBagConstraints constraints = new GridBagConstraints();

        ButtonGroup languageUsageGroup = new ButtonGroup();
        useDefaultLocale = new JRadioButton(controller.getLocaliser().getString("showPreferencesDialog.useDefault"));
        JRadioButton useSpecific = new JRadioButton(controller.getLocaliser().getString("showPreferencesDialog.useSpecific"));
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
        languageComboBox = new JComboBox();
        int numberOfLanguages = Integer
                .parseInt(controller.getLocaliser().getString("showPreferencesDialog.numberOfLanguages"));
        for (int i = 1; i <= numberOfLanguages; i++) {
            String languageCode = controller.getLocaliser().getString("showPreferencesDialog.languageCode." + i);
            String language = controller.getLocaliser().getString("showPreferencesDialog.language." + i);
            languageToLanguageCodeMap.put(language, languageCode);
            languageCodeToLanguageMap.put(languageCode, language);
            languageComboBox.addItem(language);
        }

        // get the languageCode value stored in the model
        String userLanguageCode = controller.getModel().getUserPreference(MultiBitModel.USER_LANGUAGE_CODE);
        if (userLanguageCode == null || MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            useDefaultLocale.setSelected(true);
            languageComboBox.setEnabled(false);
        } else {
            useSpecific.setSelected(true);
            languageComboBox.setSelectedItem(languageCodeToLanguageMap.get(userLanguageCode));
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
        preferencesPanel.add(languagePanel, constraints);

        return preferencesPanel;
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

    public Data getData() {
        Item languageItem = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
        if (useDefaultLocale.isSelected()) {
            languageItem.setNewValue(MultiBitModel.USER_LANGUAGE_IS_DEFAULT);
        } else {
            languageItem.setNewValue(languageToLanguageCodeMap.get(languageComboBox.getSelectedItem()));
        }

        return data;
    }
}