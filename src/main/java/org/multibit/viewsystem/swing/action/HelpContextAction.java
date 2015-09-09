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

import org.multibit.controller.Controller;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * This {@link Action} represents an action that displays a context specific help page
 */
public class HelpContextAction extends AbstractAction {

    private static final long serialVersionUID = 191948235465057705L;

    private Controller controller;
    private String helpContextToDisplay;

    /**
     * Creates a new {@link HelpContextAction}.
     * @param controller The Controller
     * @param imagePath The relative path to the image to load for the item. Usually an imageLoader constant
     * @param textKey The localisation key for the text of the action
     * @param tooltipKey The localisation key for the tooltip of the action
     * @param mnemonicKey The localisation key for the mnemonic of the action
     * @param helpContextToDisplay The help context to display on action activation.   A path in the help
     */
    public HelpContextAction(Controller controller, String imagePath, String textKey, String tooltipKey, String mnemonicKey, String helpContextToDisplay) {
        super(controller.getLocaliser().getString(textKey), ImageLoader.createImageIcon(imagePath));
        this.controller = controller;
        this.helpContextToDisplay = helpContextToDisplay;

        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString(tooltipKey)));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic(mnemonicKey));
    }

    /**
     * display the view specified
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        controller.displayHelpContext(helpContextToDisplay);
    }
}