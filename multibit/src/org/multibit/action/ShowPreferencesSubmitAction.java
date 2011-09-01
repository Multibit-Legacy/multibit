package org.multibit.action;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * an action to process the submit of the Preferences view
 * 
 * @author jim
 *
 */
public class ShowPreferencesSubmitAction implements Action {

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
    
    public ShowPreferencesSubmitAction(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller; 
        this.mainFrame = mainFrame;
    }
    
    public void execute(DataProvider dataProvider) {
        boolean updateStatusLabel = false;
        String updateStatusText = "";
        
        if (dataProvider != null) {
            Data data = dataProvider.getData();
            
            if (data != null)  {
                Item feeItem = data.getItem(MultiBitModel.SEND_FEE);
                if (feeItem != null && feeItem.getNewValue() != null) {
                    // check fee is a number
                    try {
                        Double feeAsDouble = Double.parseDouble((String)feeItem.getNewValue());
                        controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, (String)feeItem.getNewValue());
                         
                   } catch (NumberFormatException nfe) {
                        // recycle the old fee and set status message
                       controller.getModel().setUserPreference(MultiBitModel.SEND_FEE, (String)feeItem.getOriginalValue());
                       updateStatusLabel = true;
                       updateStatusText = controller.getLocaliser().getString("showPreferencesPanel.couldNotUnderstandFee", new Object[]{feeItem.getNewValue()});
                   }
 
                }
                Item languageItem = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
                if (languageItem != null && languageItem.getNewValue() != null && !languageItem.getNewValue().equals(languageItem.getOriginalValue())) {
                    // new language to set on model
                    controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE, (String)languageItem.getNewValue());
                    controller.fireLanguageChanged();
                }
            }
        }
        
        // return to parent view
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_SAME);       
 
        if (updateStatusLabel) {
            mainFrame.updateStatusLabel(updateStatusText);
        }
    }
    
    public String getDisplayText() {
        // would not normally be seen
        return "preferencesSubmit";
    }
}
