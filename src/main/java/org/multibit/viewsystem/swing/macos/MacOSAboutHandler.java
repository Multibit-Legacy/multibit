package org.multibit.viewsystem.swing.macos;

import org.multibit.controller.ActionForward;
import org.multibit.controller.MultiBitController;

import com.apple.eawt.AboutHandler;
import com.apple.eawt.AppEvent.AboutEvent;

public class MacOSAboutHandler implements AboutHandler {
    private MultiBitController controller;
    
    public MacOSAboutHandler(MultiBitController controller) {
        this.controller = controller;
    }

    @Override
    public void handleAbout(AboutEvent arg0) {
        controller.setActionForwardToSibling(ActionForward.FORWARD_TO_HELP_ABOUT);        
    }
}

