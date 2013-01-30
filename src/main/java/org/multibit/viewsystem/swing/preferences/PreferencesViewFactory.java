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
package org.multibit.viewsystem.swing.preferences;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.AbstractModularViewFactory;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.preferences.modules.AbstractPreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.BitcoinPreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.CorePreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.PreferencesModule;
import org.multibit.viewsystem.swing.preferences.modules.TickerPreferencesModule;

/**
 *
 * @author Cameron Garnham
 */
public class PreferencesViewFactory extends AbstractModularViewFactory<PreferencesPanel, AbstractPreferencesModule, PreferencesModule> {

    public PreferencesViewFactory(MultiBitController controller, MultiBitFrame mainFrame) {
        super(controller, mainFrame,PreferencesPanel.class, PreferencesModule.class);
    }

    public void addModuleFromEnum(PreferencesModule moduleEnum) {
        AbstractPreferencesModule module = null;

        switch (moduleEnum) {
            case CORE:
                module = new CorePreferencesModule(this.controller, this.mainFrame);
                break;

            case BITCOIN:
                module = new BitcoinPreferencesModule(this.controller, this.mainFrame);
                break;

            case TICKER:
                module = new TickerPreferencesModule(this.controller, this.mainFrame);
                break;
        }

        if (null != module) {
            super.addModule(module);
        }
    }

    @Override
    protected PreferencesPanel createNewModularView() {
        return new PreferencesPanel(super.controller, super.mainFrame);
    }
}
