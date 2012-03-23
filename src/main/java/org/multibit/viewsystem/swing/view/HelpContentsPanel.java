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
package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.browser.Browser;

public class HelpContentsPanel extends JPanel implements View {

    public static final String HELP_BASE_URL = "http://www.multibit.org/";
    
    public static final String HELP_SENDING_URL = "help_sendingBitcoin.html";
    public static final String HELP_RECEIVING_URL = "help_receivingBitcoin.html";
    public static final String HELP_AVAILABLE_TO_SPEND_URL = "help_availableToSpend.html";
    
    private static final long serialVersionUID = 4921443778446348403L;

    private Browser browser;
    private String helpContext;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
  
    public static final String SPACER = "   "; // 3 spaces

    boolean firstTimeLoaded = false;

    public HelpContentsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        this.helpContext = "help_contents.html";
        
        setLayout(new BorderLayout());
        firstTimeLoaded = true;

        setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        final MultiBitController  finalController = controller;
        final MultiBitFrame finalMainFrame = mainFrame;
        
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        SwingUtilities.invokeLater(new Runnable() {

            @Override
            public void run() {
                browser = new Browser(finalController, finalMainFrame, HELP_BASE_URL + helpContext);
                
                JScrollPane scrollPane = new JScrollPane(browser);
                scrollPane.setPreferredSize(new Dimension(800, 400));
                scrollPane.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0), 
                        BorderFactory.createMatteBorder(1, 0, 1, 0,  ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker())));

                add(scrollPane, BorderLayout.CENTER);          
            }
            
        });
      
    }
    

    public static String createMultilineTooltipText(String[] toolTips) {
        // multiline tool tip text
        String toolTipText = "<html><font face=\"sansserif\">";

        if (toolTips != null) {
            for (int i = 0; i < toolTips.length - 1; i++) {
                if (toolTips[i] != null && !"".equals(toolTips[i])) {
                    toolTipText = toolTipText + toolTips[i] + "<br>";
                }
            }
        }
        toolTipText = toolTipText + toolTips[toolTips.length - 1] + "</font></html>";

        return toolTipText;
    }

    @Override
    public void navigateAwayFromView() {
        controller.updateStatusLabel("");
    }

    @Override
    public void displayView() {
        String newHelpContext = mainFrame.getHelpContext();
        
        if (newHelpContext != null && !newHelpContext.equals("")) {
            helpContext = newHelpContext;
        }
        if (!firstTimeLoaded) {
            if (browser != null) {
                browser.visit(HELP_BASE_URL + helpContext);
            }
        }
        firstTimeLoaded = false;
    }
    
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.HELP_CONTENTS_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("showHelpContentsAction.text");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("showHelpContentsAction.tooltip");
    }

    @Override
    public int getViewId() {
        return View.HELP_CONTENTS_VIEW;
    }
}