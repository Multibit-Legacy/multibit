package org.multibit;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Locale;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.apache.log4j.xml.DOMConfigurator;
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
    
    private static Logger logger;

    
    /**
     * start multibit user interface
     */
    public static void main(String args[]) {
        System.out.println("MultiBit#main PING 1");
        // initialise log4j
        DOMConfigurator.configure("log4j.xml");
        logger = Logger.getLogger(MultiBit.class.getName());
        System.out.println("MultiBit#main PING 2");
               
        // create the controller
        MultiBitController controller = new MultiBitController();
        System.out.println("MultiBit#main PING 3");
        
        // load up the user preferences
        Properties userPreferences = loadUserPreferences();
        System.out.println("MultiBit#main PING 4");
        
        // if test or production is not specified, default to production
        String testOrProduction = userPreferences.getProperty(MultiBitModel.TEST_OR_PRODUCTION_NETWORK);
        if (testOrProduction == null) {
            testOrProduction = MultiBitModel.PRODUCTION_NETWORK_VALUE;
            userPreferences.put(MultiBitModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
        }
        boolean useTestNet = MultiBitModel.TEST_NETWORK_VALUE.equals(testOrProduction);
        logger.debug("useTestNet = " + useTestNet);
        System.out.println("MultiBit#main PING 5");
        
        Localiser localiser;
        String userLanguageCode = userPreferences.getProperty(MultiBitModel.USER_LANGUAGE_CODE);
        logger.debug("userLanguageCode = " + userLanguageCode);
        System.out.println("MultiBit#main PING 6");
        
        if (userLanguageCode == null || MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, Locale.getDefault());
        } else {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale(userLanguageCode));       
        }
        controller.setLocaliser(localiser);
        System.out.println("MultiBit#main PING 7");
        
        // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel(controller, userPreferences);
        controller.setModel(model);
                
        System.out.println("MultiBit#main PING 8.1");
        
        // create the view systems
        // add the swing view system
        ViewSystem swingView = new MultiBitFrame(controller);
        System.out.println("MultiBit#main PING 8.2");
        controller.registerViewSystem(swingView);
        System.out.println("MultiBit#main PING 8.3");
        
        // show the home page
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_HOME_PAGE);

        System.out.println("MultiBit#main PING 9");
        
        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(useTestNet, controller);
        controller.setMultiBitService(multiBitService);
        System.out.println("MultiBit#main PING 10");
        
        // TODO make more generic
        ((MultiBitFrame)swingView).fireDataChanged();
        multiBitService.downloadBlockChain();
        System.out.println("MultiBit#main PING 11");     
    }
    
    private static Properties loadUserPreferences() {
        Properties userPreferences = new Properties();
        try {
            InputStream inputStream = new FileInputStream(MultiBit.PROPERTIES_FILE_NAME);
            InputStreamReader inputStreamReader = new InputStreamReader(inputStream, "UTF8");
            userPreferences.load(inputStreamReader);
        } catch (FileNotFoundException e) {
            // ok - may not have been created yet
        } catch (IOException e) {
            // ok may not be written yet
        }
        
        return userPreferences;
    }
}
