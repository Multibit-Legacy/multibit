package org.multibit;

/**
 * Copyright 2011 multibit.org
 *
 * Licensed under the MIT license (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://opensource.org/licenses/mit-license.php
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Locale;
import java.util.Properties;

import org.apache.log4j.xml.DOMConfigurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.network.FileHandler;
import org.multibit.network.MultiBitService;
import org.multibit.viewsystem.ViewSystem;

import javax.swing.UIManager;
import javax.swing.UIManager.*;



/**
 * Main MultiBit entry class
 * 
 * @author jim
 * 
 */
public class MultiBit {
    private static Logger logger;

    /**
     * start multibit user interface
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static void main(String args[]) {
        // initialise log4j
        DOMConfigurator.configure("log4j.xml");
        logger = LoggerFactory.getLogger(MultiBit.class.getName());

        // load up the user preferences
        Properties userPreferences = FileHandler.loadUserPreferences();

        // create the controller
        MultiBitController controller = new MultiBitController(userPreferences);

        // if test or production is not specified, default to production
        String testOrProduction = userPreferences.getProperty(MultiBitModel.TEST_OR_PRODUCTION_NETWORK);
        if (testOrProduction == null) {
            testOrProduction = MultiBitModel.PRODUCTION_NETWORK_VALUE;
            userPreferences.put(MultiBitModel.TEST_OR_PRODUCTION_NETWORK, testOrProduction);
        }
        boolean useTestNet = MultiBitModel.TEST_NETWORK_VALUE.equals(testOrProduction);
        logger.debug("useTestNet = " + useTestNet);

        Localiser localiser;
        String userLanguageCode = userPreferences.getProperty(MultiBitModel.USER_LANGUAGE_CODE);
        logger.debug("userLanguageCode = " + userLanguageCode);

        if (userLanguageCode == null || MultiBitModel.USER_LANGUAGE_IS_DEFAULT.equals(userLanguageCode)) {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, Locale.getDefault());
        } else {
            localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, new Locale(userLanguageCode));
        }
        controller.setLocaliser(localiser);

        // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel(controller, userPreferences);
        controller.setModel(model);

        String operatingSystem = System.getProperty("os.name");
        operatingSystem = "Jim";
        try {
            // Nimbus for everything other than the Mac
            for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                if (("Mac OS X".equals(operatingSystem) && "Mac OS X".equals(info.getName())) ||
                        (!"Mac OS X".equals(operatingSystem) && "Nimbus".equals(info.getName()))) {
                    UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (Exception e) {
            // If Nimbus is not available, you can set the GUI to another look and feel.
        }
        
        // create the view systems
        // add the swing view system
        // this is done by reflection just to keep it out the import
        ViewSystem swingViewSystem = null;
        try {
            Class multiBitFrameClass = Class.forName("org.multibit.viewsystem.swing.MultiBitFrame");
            Constructor controllerConstructor = multiBitFrameClass.getConstructor(new Class[] { MultiBitController.class});
            
            swingViewSystem = (ViewSystem) createObject(controllerConstructor, new Object[] {controller});
            controller.registerViewSystem(swingViewSystem);
         } catch (ClassNotFoundException e) {
            System.out.println(e);
          } catch (NoSuchMethodException e) {
            System.out.println(e);
          }
 
        // create the MultiBitService that connects to the bitcoin network
        MultiBitService multiBitService = new MultiBitService(useTestNet, controller);
        controller.setMultiBitService(multiBitService);

        // make sure the total is updated
        controller.fireDataChanged();
        
        // display the next view
        controller.displayNextView(ViewSystem.NEW_VIEW_IS_SIBLING_OF_PREVIOUS);

        multiBitService.downloadBlockChain();
    }
    

    @SuppressWarnings("rawtypes")
    public static Object createObject(Constructor constructor,
        Object[] arguments) {

       Object object = null;

      try {
        object = constructor.newInstance(arguments);
        return object;
      } catch (InstantiationException e) {
        System.out.println(e);
      } catch (IllegalAccessException e) {
        System.out.println(e);
      } catch (IllegalArgumentException e) {
        System.out.println(e);
      } catch (InvocationTargetException e) {
        System.out.println(e);
      }
      return object;
    }
}
