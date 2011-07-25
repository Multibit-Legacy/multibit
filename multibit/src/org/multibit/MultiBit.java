package org.multibit;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * Main MultiBit entry class
 * 
 * @author jim
 * 
 */
public class MultiBit {
    public static final String PROPERTIES_FILE_NAME = "multibit.properties";
    public static final String PROPERTIES_HEADER_TEXT= "multibit";
    
    /**
     * start multibit user interface
     */
    public static void main(String args[]) {
        // create the controller
        MultiBitController controller = new MultiBitController();
        
         // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel();
        controller.setModel(model);
               
        // load up the user preferences
        Properties userPreferences = loadUserPreferences();
        model.setAllUserPreferences(userPreferences);
 
        Localiser localiser;
        String userLanguageCode = controller.getModel().getUserPreference(MultiBitModel.USER_LANGUAGE_CODE);
        if (MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, Locale.getDefault());
        } else {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale(userLanguageCode));       
        }
        controller.setLocaliser(localiser);

 
        // create the view systems and set the localisers
        // add the command line view system
        ViewSystem textView = new CommandLineViewSystem(controller);
        controller.registerViewSystem(textView);

        // add the swing view system
        ViewSystem swingView = new MultiBitFrame(controller);
        controller.registerViewSystem(swingView);
        
        // show the home page
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_HOME_PAGE);
    }
    
    private static Properties loadUserPreferences() {
        Properties userPreferences = new Properties();
        try {
            InputStream inputStream = new FileInputStream(MultiBit.PROPERTIES_FILE_NAME);
            userPreferences.load(inputStream);
        } catch (FileNotFoundException e) {
            // ok - may not have been created yet
        } catch (IOException e) {
            // ok may not be written yet
        }
        
        return userPreferences;
    }
}
