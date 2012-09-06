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
package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * The Charts view.
 */
public class ChartsPanel extends JPanel implements View {
    private static final long serialVersionUID = 191352212345998705L;
  
    private MultiBitController controller;
    
    // dummy label for illustration
    private JLabel walletLabel;

  /**
     * Creates a new {@link ChartsPanel}.
     */
    public ChartsPanel(MultiBitController controller, MultiBitFrame mainFrame) {        
        this.controller = controller;
        
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);        
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        initUI();
    }
    
    private void initUI() {
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setOpaque(false);
        mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(createChartPanel(), constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        add(mainScrollPane, BorderLayout.CENTER);
    }

    /**
     * Create a panel containing the chart to show.
     */
    private JPanel createChartPanel() {
        JPanel chartPanel = new JPanel();
        chartPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        chartPanel.setLayout(new BorderLayout());

        walletLabel = new JLabel();
        walletLabel.setText(controller.getModel().getActiveWalletFilename());
        chartPanel.add(walletLabel, BorderLayout.CENTER);
        
        return chartPanel;
    }
    
    /**
     * Update the chart panel (active wallet may have changed).
     */
    private void updateChart() {
        walletLabel.setText(controller.getModel().getActiveWalletFilename());        
    }

    @Override
    /**
     * Release any resources used when user navigates away from this view.
     */
    public void navigateAwayFromView() {
    }

    @Override
    public void displayView() {  
        updateChart();
    }
       
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.CHART_LINE_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("chartsPanelAction.text");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("chartsPanelAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.CHARTS_VIEW;
    }
}