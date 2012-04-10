/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing.view.ticker;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ticker view
 */
public class TickerPanel extends JPanel {

    private static final Logger log = LoggerFactory.getLogger(TickerPanel.class);

    private static final long serialVersionUID = 191356387345057705L;
    
    private TickerTablePanel tickerTablePanel;

    private MultiBitController controller;
    
    private MultiBitFrame mainFrame;

  /**
     * Creates a new {@link TickerPanel}.
     */
    public TickerPanel(MultiBitController controller, MultiBitFrame mainFrame) { 
    	
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        this.controller = controller;
        this.mainFrame = mainFrame;
        
        setOpaque(true);

        setLayout(new BorderLayout());

        add(createTickerPanel(), BorderLayout.CENTER);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }
    
    private JPanel createTickerPanel() {
    	
        JPanel tickerPanel = new JPanel();
        tickerPanel.setPreferredSize(new Dimension(300,50));
        
        GridBagConstraints constraints = new GridBagConstraints();
        
        tickerTablePanel = new TickerTablePanel(mainFrame, controller);
        tickerTablePanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        tickerTablePanel.setOpaque(true);
        tickerPanel.add(tickerTablePanel, constraints);

        return tickerPanel;      
    }
    
    public void updatePanel(){
        tickerTablePanel.update();  //.setText("MtGox: " + this.controller.getModel().getExchangeData().getLastTickUSD());
    }
}