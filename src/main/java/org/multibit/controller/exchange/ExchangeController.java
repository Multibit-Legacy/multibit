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
package org.multibit.controller.exchange;

import java.net.URI;
import org.multibit.controller.AbstractController;
import org.multibit.controller.AbstractEventHandeler;
import org.multibit.controller.core.CoreController;
import org.multibit.model.bitcoin.BitcoinModel;
import org.multibit.model.exchange.ExchangeModel;
import org.multibit.viewsystem.swing.action.ExitAction;

/**
 *
 * @author Cameron Garnham
 */
public class ExchangeController extends AbstractController<CoreController> {

    
    private final EventHandeler eventHandeler;
    
    /**
     * The data model backing the views.
     */
    private ExchangeModel model;
    
    
    public ExchangeController(CoreController coreController){
        super(coreController);
        
        this.eventHandeler = new EventHandeler(this);
        super.addEventHandler(this.getEventHandeler());
    }
    
    
    @Override
    public final AbstractEventHandeler getEventHandeler() {
        return this.eventHandeler;
    }

    @Override
    public ExchangeModel getModel() {
        return model;
    }

    public void setModel(ExchangeModel model) {
        this.model = model;
    }
    
    
    private class EventHandeler extends AbstractEventHandeler<ExchangeController> {

        private volatile URI rawBitcoinURI = null;

        public EventHandeler(ExchangeController controller) {
            super(controller);
        }

        @Override
        public void handleOpenURIEvent(URI rawBitcoinURI) {
            this.rawBitcoinURI = rawBitcoinURI;
            // do nothing

        }

        @Override
        public void handleQuitEvent(ExitAction exitAction) {
            // do nothing
        }
    }
    
}
