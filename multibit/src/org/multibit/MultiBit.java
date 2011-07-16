package org.multibit;

import java.util.Locale;

import javax.swing.JFrame;

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

        // display UI
        //System.out.println("MultiBit : filename = '" + filename + "', locale = '" + locale + "'");
        JFrame multiBitFrame = new MultiBitFrame(filename, localiser);
    }
}
