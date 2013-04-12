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
package org.multibit;

import java.util.Locale;
import org.multibit.controller.MultiBitController;
import org.multibit.exchange.CurrencyConverter;
import org.multibit.model.MultiBitModel;

/**
 *
 * @author Cameron Garnham
 */
public class CreateControllers {

    public static Controllers createControllers() {
        return CreateControllers.createControllers(null, null, null, null);
    }

    public static Controllers createControllers(Localiser localiser) {
        return CreateControllers.createControllers(localiser, null, null, null);
    }
    
    public static Controllers createControllers(Localiser localiser, ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        return CreateControllers.createControllers(localiser, applicationDataDirectoryLocator, null, null);
    }
    
    public static Controllers createControllers(ApplicationDataDirectoryLocator applicationDataDirectoryLocator) {
        return CreateControllers.createControllers(null, applicationDataDirectoryLocator, null, null);
    }

    public static Controllers createControllers(
            Localiser localiser,
            ApplicationDataDirectoryLocator applicationDataDirectoryLocator,
            String first,
            String second
            )
    {

        final MultiBitController multiBitController = new MultiBitController(applicationDataDirectoryLocator);
        final MultiBitModel multiBitModel = new MultiBitModel(multiBitController);
        if (null == multiBitModel) {
            return null;
        }
        multiBitController.setLocaliser((null != localiser) ? localiser : new Localiser(Locale.ENGLISH));
        multiBitController.setModel(multiBitModel);
        
        CurrencyConverter.INSTANCE.initialise(multiBitController);

        MultiBit.setController(multiBitController);
        
        return new Controllers(multiBitController);
    }

    public static class Controllers {

        public final MultiBitController multiBitController;

        public Controllers(final MultiBitController multiBitController) {
            this.multiBitController = multiBitController;
        }
    }
}
