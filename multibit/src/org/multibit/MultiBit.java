package org.multibit;

import java.util.Locale;

import org.multibit.viewsystem.Localiser;
import org.multibit.viewsystem.ViewSystem;
import org.multibit.viewsystem.commandline.CommandLineViewSystem;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * Class for displaying the contents of a Wallet
 * 
 * @author jim
 * 
 */
public class MultiBit {
    /**
     * start multibit user interface
     * 
     * @param args
     *            Parameter 1: locale to use (for testing) e.g. es_ES
     *            Parameter 2: name of wallet file to open 
     */
    public static void main(String args[]) {
        Localiser localiser = null;

        String filename = null;

        Locale locale = Locale.getDefault();
        if (args.length == 1) {
            locale = new Locale(args[0]);
            Locale.setDefault(locale);
        } else {
            if (args.length >= 2) {
                locale = new Locale(args[0]);
                Locale.setDefault(locale);
                filename = args[1];
            }
        }

        localiser = new Localiser(Localiser.VIEWER_RESOURCE_BUNDLE_NAME, locale);

        if (args.length < 1 || args.length > 2) {
            System.out.println(localiser.getString("multiBit.usageNote"));
        }

        // create the controller
        MultiBitController controller = new MultiBitController();
        
        // create the views and set the localisers
        // add the command line view
        ViewSystem textView = new CommandLineViewSystem(controller, localiser);
        controller.registerViewSystem(textView);

        ViewSystem swingView = new MultiBitFrame(controller, filename, localiser);
        controller.registerViewSystem(swingView);
        
        // show the home page
        controller.setActionForward(ActionForward.FORWARD_TO_HOME_PAGE);
    }
}
