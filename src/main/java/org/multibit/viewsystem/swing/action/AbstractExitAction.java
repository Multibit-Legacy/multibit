/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.viewsystem.swing.action;

import javax.swing.AbstractAction;
import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

/**
 *
 * @author Cameron Garnham
 */
public abstract class AbstractExitAction extends AbstractAction {
    protected final Controller controller;

    public AbstractExitAction(Controller controller) {
        super(controller.getLocaliser().getString("exitAction.text"));
        this.controller = controller;
        
        MnemonicUtil mnemonicUtil = new MnemonicUtil(this.controller.getLocaliser());
        putValue(SHORT_DESCRIPTION, HelpContentsPanel.createTooltipTextForMenuItem(this.controller.getLocaliser().getString("exitAction.tooltip")));
        putValue(MNEMONIC_KEY, mnemonicUtil.getMnemonic("exitAction.mnemonicKey"));
    }
    
}
