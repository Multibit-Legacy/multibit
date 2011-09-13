package org.multibit.viewsystem.swing.action;

import java.util.MissingResourceException;

import javax.swing.KeyStroke;

import org.multibit.Localiser;

public class MnemonicUtil {

    private Localiser localiser;

    public MnemonicUtil(Localiser localiser) {
        this.localiser = localiser;
    }

    /**
     * get the mnemonic key code for the passed in internationalisation key
     * 
     * @param key
     * @return
     */
    public int getMnemonic(String key) {
        if (localiser != null) {
            try {
                return KeyStroke.getKeyStroke(localiser.getString(key)).getKeyCode();
            } catch (NullPointerException npe) {
                return 0;
            } catch (ClassCastException cce) {
                return 0;
            } catch (MissingResourceException mre) {
                return 0;
            }
        } else {
            return 0;
        }
    }
}
