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
package org.multibit.viewsystem.core;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.WelcomePanel;
import org.multibit.viewsystem.swing.view.panels.HelpAboutPanel;
import org.multibit.viewsystem.swing.view.panels.PreferencesPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.CoreFrame;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * a factory class that lazy loads views
 * 
 * @author jim
 * 
 */
public class CoreViewFactory extends AbstractViewFactory {

    Controller controller;
    CoreFrame mainFrame;
    
    public CoreViewFactory(Controller controller, CoreFrame mainFrame) {
        super();
        this.controller = controller;
        this.mainFrame = mainFrame;
    }
    
    @Override
    public View createView(int viewNumber) {
        View viewToReturn = null;
        switch (viewNumber) {
            case View.SAME_VIEW:
                {
                    assert false;
                    break;
                }
            case View.WELCOME_VIEW:
                {
                    viewToReturn = new WelcomePanel(controller);
                    break;
                }
            case View.HELP_ABOUT_VIEW:
                {
                    viewToReturn = new HelpAboutPanel(controller);
                    break;
                }
            case View.HELP_CONTENTS_VIEW:
                {
                    viewToReturn = new HelpContentsPanel(controller, mainFrame);
                    break;
                }
            case View.PREFERENCES_VIEW:
                {
                    PreferencesPanel preferencesPanel = new PreferencesPanel(controller, mainFrame);
                    viewToReturn = preferencesPanel;
                    break;
                }
            default:
                {
                }
        }
        if (viewToReturn != null) {
            viewMap.put(viewNumber, viewToReturn);
        }
        return viewToReturn;
    }
    
    
    
}
