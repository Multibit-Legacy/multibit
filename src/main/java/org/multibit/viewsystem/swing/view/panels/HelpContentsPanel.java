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
package org.multibit.viewsystem.swing.view.panels;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.MultiBit;
import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.browser.Browser;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class HelpContentsPanel extends JPanel implements Viewable {

    public static final String HELP_BASE_URL = "http://www.multibit.org/v0.5/";
    
    public static final String HELP_AVAILABLE_TO_SPEND_URL = "help_availableToSpend.html";
    public static final String HELP_CONTENTS_URL = "help_contents.html";
    public static final String HELP_EXPORTING_PRIVATE_KEYS_URL = "help_exportingPrivateKeys.html";
    public static final String HELP_IMPORTING_PRIVATE_KEYS_URL = "help_importingPrivateKeys.html";
    public static final String HELP_RECEIVING_URL = "help_receivingBitcoin.html";
    public static final String HELP_SENDING_URL = "help_sendingBitcoin.html";
    public static final String HELP_RESET_BLOCKCHAIN_URL = "help_resetBlockchain.html";
    public static final String HELP_WALLET_FORMATS_URL = "help_walletFormats.html";
    public static final String HELP_WALLET_TYPES_URL = "help_walletTypes.html";
    
    private static final long serialVersionUID = 4921443778446348403L;

    private static Browser browser;
    private String helpContext;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;
  
    public static final String SPACER = "   "; // 3 spaces

    boolean firstTimeLoaded = false;

    public HelpContentsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        helpContext = mainFrame.getHelpContext();
        if (helpContext == null || "".equals(helpContext)) {
            helpContext = HELP_CONTENTS_URL;
        }

        setLayout(new BorderLayout());
        firstTimeLoaded = true;

        setBackground(ColorAndFontConstants.BACKGROUND_COLOR);

        final MultiBitController finalController = controller;
        final MultiBitFrame finalMainFrame = mainFrame;

        mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        browser = new Browser(finalController, finalMainFrame, HELP_BASE_URL + helpContext);

        JScrollPane scrollPane = new JScrollPane(browser);
        scrollPane.setPreferredSize(new Dimension(800, 400));
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getHorizontalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        scrollPane.getVerticalScrollBar().setUnitIncrement(MultiBitModel.SCROLL_INCREMENT);
        add(scrollPane, BorderLayout.CENTER);
    }
    

    public static String createMultilineTooltipText(String[] toolTips) {
        // Multiline tool tip text.
        int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
        boolean isItalic = false;
        boolean isBold = false;
        FontSizer.INSTANCE.initialise(MultiBit.getController());
        Font adjustedFont = FontSizer.INSTANCE.getAdjustedDefaultFont();
        if (adjustedFont != null) {
            fontSize = adjustedFont.getSize();
            isItalic = adjustedFont.isItalic();
            isBold = adjustedFont.isBold();
        }
        
        String fontCSS = "font-size:" + fontSize + "pt; font-family:" + adjustedFont.getFamily() + ";";
        if (isItalic) {
            fontCSS = fontCSS + "font-style:italic;";
        } else {
            fontCSS = fontCSS + "font-style:normal;";
        }
        if (isBold) {
            fontCSS = fontCSS + "font-weight:bold;";
        } else {
            fontCSS = fontCSS + "font-weight:normal;";
        }
        String toolTipText = "<html><font face=\"sansserif\" style= \"" + fontCSS + "\">";

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
        MessageManager.INSTANCE.addMessage(new Message(" "));
    }

    @Override
    public void displayView() {
        if (browser == null) {
            browser = new Browser(controller, mainFrame, HELP_BASE_URL + helpContext);
        }
        
        helpContext = mainFrame.getHelpContext();
        if (helpContext == null || "".equals(helpContext)) {
            helpContext = "help_contents.html";
        }
        
        if (browser != null) {
            if (!firstTimeLoaded || (firstTimeLoaded && !browser.isLoading())) {
                  browser.visit(HELP_BASE_URL + helpContext, false);
            }
        }
        firstTimeLoaded = false;
    }
    
    public static void clearBrowser() {
        browser = null;
    }
    
    @Override
    public Icon getViewIcon() {
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            return ImageLoader.createImageIcon(ImageLoader.HELP_CONTENTS_ICON_FILE);
        } else {
            return ImageLoader.createImageIcon(ImageLoader.HELP_CONTENTS_RTL_ICON_FILE);
        }
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
    public View getViewId() {
        return View.HELP_CONTENTS_VIEW;
    }
}