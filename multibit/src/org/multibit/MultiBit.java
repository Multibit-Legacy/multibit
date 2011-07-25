package org.multibit;

import java.util.Locale;

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
    /**
     * start multibit user interface
     * 
     */
    public static void main(String args[]) {
        // create the controller
        MultiBitController controller = new MultiBitController();
        
        Localiser localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, Locale.getDefault());
        controller.setLocaliser(localiser);

         // create the model and put it in the controller
        MultiBitModel model = new MultiBitModel();
        controller.setModel(model);
               
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
}
