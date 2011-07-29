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
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.ViewSystem;
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

        // load up the user preferences
        Properties userPreferences = loadUserPreferences();

        // if test or production is not specified, default to production
        String testOrProduction = userPreferences.getProperty(MultiBitModel.TEST_OR_PRODUCTION_NETWORK);
        if (testOrProduction == null) {
            testOrProduction = MultiBitModel.PRODUCTION_NETWORK_VALUE;
            userPreferences.put(MultiBitModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
        }
        boolean useTestNet = testOrProduction == MultiBitModel.TEST_NETWORK_VALUE;
        
        Localiser localiser;
        String userLanguageCode = userPreferences.getProperty(MultiBitModel.USER_LANGUAGE_CODE);
        if (MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, Locale.getDefault());
        } else {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale(userLanguageCode));       
        }
        controller.setLocaliser(localiser);

        // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel(controller, userPreferences);
        controller.setModel(model);
                

        // create the view systems
        
        // add the command line view system
        //ViewSystem textView = new CommandLineViewSystem(controller);
        //controller.registerViewSystem(textView);

        // add the swing view system
        ViewSystem swingView = new MultiBitFrame(controller);
        controller.registerViewSystem(swingView);
        
        // show the home page
        controller.setActionForwardToChild(ActionForward.FORWARD_TO_HOME_PAGE);

        
        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(useTestNet, controller);
        controller.setMultiBitService(multiBitService);
        
        // TODO make more generic
        ((MultiBitFrame)swingView).fireDataChanged();
        multiBitService.downloadBlockChain();
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
