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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.browser.Browser;

public class HelpContentsPanel extends JPanel implements View {

    private static final long serialVersionUID = 4921443778446348403L;

    private Browser browser;
    private String initialUrl;

    private MultiBitController controller;
  
    public static final String SPACER = "   "; // 3 spaces

    boolean firstTimeLoaded = false;

    public HelpContentsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.initialUrl = "http://www.multibit.org/help_contents.html";

        setLayout(new BorderLayout());
        firstTimeLoaded = true;

        setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        final MultiBitController  finalController = controller;
        final MultiBitFrame finalMainFrame = mainFrame;
        
        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        SwingUtilities.invokeLater(new Runnable() {

            @Override
            public void run() {
                browser = new Browser(finalController, finalMainFrame, initialUrl);
                
                JScrollPane scrollPane = new JScrollPane(browser);
                scrollPane.setPreferredSize(new Dimension(800, 400));
                scrollPane.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0), 
                        BorderFactory.createMatteBorder(1, 0, 1, 0,  ColorAndFontConstants.DARK_BACKGROUND_COLOR.darker())));

                add(scrollPane, BorderLayout.CENTER);          
            }
            
        });
      
    }

    @Override
    public void displayView() {
        if (!firstTimeLoaded) {
            if (browser != null) {
                browser.visit(initialUrl);
            }
        }
        firstTimeLoaded = false;
    }

    @Override
    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        controller.updateStatusLabel("");
    }

    @Override
    public void updateView() {
        // TODO Auto-generated method stub
        
    }
}