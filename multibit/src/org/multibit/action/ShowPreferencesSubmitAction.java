package org.multibit.action;

import org.multibit.controller.MultiBitController;
import org.multibit.model.Data;
import org.multibit.model.DataProvider;
import org.multibit.model.Item;
import org.multibit.model.MultiBitModel;

/**
 * an action to process the submit of the Preferences view
 * 
 * @author jim
 *
 */
public class ShowPreferencesSubmitAction implements Action {

    private MultiBitController controller;
    
    public ShowPreferencesSubmitAction(MultiBitController controller) {
        this.controller = controller;   
    }
    
    public void execute(DataProvider dataProvider) {
        if (dataProvider != null) {
            Data data = dataProvider.getData();
            
            if (data != null)  {
                Item item = data.getItem(MultiBitModel.USER_LANGUAGE_CODE);
                if (item != null && item.getNewValue() != null && !item.getNewValue().equals(item.getOriginalValue())) {
                    // new language to set on model
                    controller.getModel().setUserPreference(MultiBitModel.USER_LANGUAGE_CODE, (String)item.getNewValue());
                    controller.fireLanguageChanged();
                }
            }
        }
        
        // return to parent view
        controller.setActionForwardToParent();       
    }
    
    public String getDisplayText() {
        // would not normally be seen
        return "preferencesSubmit";
    }
}
