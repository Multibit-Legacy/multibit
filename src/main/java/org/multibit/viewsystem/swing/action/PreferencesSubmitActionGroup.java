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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import java.util.HashSet;
import java.util.Set;
import org.multibit.controller.Controller;

/**
 * This {@link Action} applies changes to the preferences panel.
 */
public class PreferencesSubmitActionGroup extends AbstractAction {

    private static final long serialVersionUID = 1923492460523457765L;
    
    private Set<PreferencesSubmitAction> preferencesSubmitActions;

    /**
     * Creates a new {@link ShowPreferencesSubmitAction}.
     */
    public PreferencesSubmitActionGroup(Controller controller, Icon icon) {
        super(controller.getLocaliser().getString("showPreferencesSubmitAction.text"), icon);

        
        preferencesSubmitActions = new HashSet<PreferencesSubmitAction>();
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, controller.getLocaliser().getString("showPreferencesSubmitAction.tooltip"));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("showPreferencesSubmitAction.mnemonicKey"));
    }

    /**
     * Change preferences.
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        
        for(PreferencesSubmitAction preferencesSubmitAction : preferencesSubmitActions)
        {
            preferencesSubmitAction.actionPerformed(event);
        }
    }
    
    
    public void addPreferencesSubmitAction(PreferencesSubmitAction preferencesSubmitAction)
    {
        preferencesSubmitActions.add(preferencesSubmitAction);
    }
    
}