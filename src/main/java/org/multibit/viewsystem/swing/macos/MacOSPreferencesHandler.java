package org.multibit.viewsystem.swing.macos;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

import com.apple.eawt.AppEvent.PreferencesEvent;
import com.apple.eawt.PreferencesHandler;

public class MacOSPreferencesHandler implements PreferencesHandler {
    private MultiBitController controller;
    
    public MacOSPreferencesHandler(MultiBitController controller) {
        this.controller = controller;
    }

    @Override
    public void handlePreferences(PreferencesEvent arg0) {
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_PREFERENCES);        
    }
}

