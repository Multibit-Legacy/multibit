package org.multibit.viewsystem.swing.macos;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.action.ExitAction;

import com.apple.eawt.AppEvent.QuitEvent;
import com.apple.eawt.QuitHandler;
import com.apple.eawt.QuitResponse;

public class MacOSQuitHandler implements QuitHandler {
    private MultiBitController controller;
    
    public MacOSQuitHandler(MultiBitController controller) {
        this.controller = controller;
    }

    @Override
    public void handleQuitRequestWith(QuitEvent arg0, QuitResponse arg1) {
        ExitAction exitAction =new ExitAction(controller);
        exitAction.actionPerformed(null);
    }
}

