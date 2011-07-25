package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.action.CancelBackToParentAction;
import org.multibit.viewsystem.swing.action.ShowPreferencesSubmitAction;

public class ShowPreferencesDialog extends MultiBitDialog implements DataProvider{

    private static final long serialVersionUID = 2065108834587842662L;
    
    private MultiBitController controller;
    
    private Map<String, String> languageToLanguageCodeMap;
    private Map<String, String> languageCodeToLanguageMap;
    private JRadioButton useDefaultLocale; 
    private JComboBox languageComboBox;
    
    private Data data;
       
    public ShowPreferencesDialog(JFrame mainFrame, MultiBitController controller) {
        super(mainFrame);
        this.controller = controller;

        languageToLanguageCodeMap = new HashMap<String, String>();
        languageCodeToLanguageMap = new HashMap<String, String>();
        
        data = new Data();

        initUI();

        pack();
        
        //copyAddressButton.requestFocusInWindow();
    }

    private void initUI() {
        positionDialogRelativeToParent(this, 0.25D, 0.25D);
        setMinimumSize(new Dimension(550, 160));
        setTitle(controller.getLocaliser().getString("showPreferencesDialog.title"));
        setLayout(new BorderLayout());
        add(createPreferencesPanel(), BorderLayout.CENTER);
        add(createButtonPanel(), BorderLayout.SOUTH);
    }

    private JPanel createPreferencesPanel() {       
        JPanel preferencesPanel = new JPanel();
        preferencesPanel.setLayout(new GridBagLayout());
               
        // language radios
        JPanel languagePanel = new JPanel(new GridBagLayout());
        languagePanel.setBorder(BorderFactory.createTitledBorder(controller.getLocaliser().getString("showPreferencesDialog.languageTitle")));
 
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
        int numberOfLanguages = Integer.parseInt(controller.getLocaliser().getString("showPreferencesDialog.numberOfLanguages"));
        for (int i=1; i<= numberOfLanguages; i++) {
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

        CancelBackToParentAction cancelAction = new CancelBackToParentAction(controller);
        JButton cancelButton = new JButton(cancelAction);
        buttonPanel.add(cancelButton);

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