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
package org.multibit.viewsystem.swing.action;

import org.multibit.Localiser;
import org.multibit.controller.Controller;
import org.multibit.utils.SafeDesktop;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.net.URI;

/**
 * This {@link javax.swing.Action} represents an action that displays a URL in an external browser
 */
public class LaunchTryMultiBitHDAction extends AbstractAction {

    private static final long serialVersionUID = 191948287465057705L;

    public static final String TRY_MULTIBIT_HD_PAGE = "https://multibit.org/try-multibit-hd.html";

    /**
     * Creates a new {@link LaunchTryMultiBitHDAction}.
     * @param controller The Controller
     * @param textKey The localisation key for the text of the action
     * @param tooltipKey The localisation key for the tooltip of the action
     */
    public LaunchTryMultiBitHDAction(Controller controller, String textKey, String tooltipKey) {
        Localiser localiser = controller.getLocaliser();
        putValue(Action.NAME, localiser.getString(textKey));
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(localiser.getString(tooltipKey)));
    }

    /**
     * Open an external browser to the 'Try MultiBit HD' page
     */
    @Override
    public void actionPerformed(ActionEvent e) {
          // Attempt to open the URI
         if (!SafeDesktop.browse(URI.create(TRY_MULTIBIT_HD_PAGE))) {
             Toolkit.getDefaultToolkit().beep();
         }
    }
}