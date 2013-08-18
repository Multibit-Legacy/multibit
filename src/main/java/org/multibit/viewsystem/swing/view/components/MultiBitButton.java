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
package org.multibit.viewsystem.swing.view.components;

import javax.swing.Action;
import javax.swing.JButton;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 * button used in MultiBit Swing UI
 * @author jim
 *
 */
public class MultiBitButton extends JButton {

    private static final long serialVersionUID = 5632457290711815650L;
    
    public MultiBitButton(Action action, Controller controller) {
        super(action);
              
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        setOpaque(false);
        setRolloverEnabled(true);
        setToolTipText(HelpContentsPanel.createTooltipText((String)action.getValue(Action.SHORT_DESCRIPTION)));
    }
    
    public MultiBitButton(String label) {
        super(label);
        
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        setOpaque(false);
        setRolloverEnabled(true);
        setToolTipText(HelpContentsPanel.createTooltipText(label));
    }
}
