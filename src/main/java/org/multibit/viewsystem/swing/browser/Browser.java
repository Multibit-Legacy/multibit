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
package org.multibit.viewsystem.swing.browser;

import java.awt.Cursor;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.ExecutionException;

import javax.swing.SwingWorker;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLEditorKit;

import org.multibit.controller.MultiBitController;
import org.multibit.message.Message;
import org.multibit.message.MessageManager;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;

public class Browser extends javax.swing.JEditorPane {
    private static final long serialVersionUID = 1L;

    private String loadingMessage;
    
    private String currentHref;
    
    private boolean loading = false;
    
    private boolean loadedOkAtConstruction;
    
    private MultiBitFrame mainFrame;
    
    public Browser(MultiBitController controller, MultiBitFrame mainFrame, String currentHref) {
        super();
        
        this.currentHref = currentHref;
        this.mainFrame = mainFrame;

        loadedOkAtConstruction = false;

        try {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            }
            addHyperlinkListener(new ActivatedHyperlinkListener(mainFrame, this, currentHref));

            loadingMessage = controller.getLocaliser().getString("browser.loadingMessage");
            setEditable(false);
            setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);

            String fontName = controller.getModel().getUserPreference(MultiBitModel.FONT_NAME);
            if (fontName == null || "".equals(fontName)) {
                fontName = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_NAME;
            }
            // Add in san-serif as a fallback.
            fontName = fontName + ", san-serif";

            int fontSize = ColorAndFontConstants.MULTIBIT_DEFAULT_FONT_SIZE;
            String fontSizeString = controller.getModel().getUserPreference(MultiBitModel.FONT_SIZE);
            if (fontSizeString != null && !"".equals(fontSizeString)) {
                try {
                    fontSize = Integer.parseInt(fontSizeString);
                } catch (NumberFormatException nfe) {
                // Use default.
                }
            }
        
            HTMLEditorKit kit = new HTMLEditorKit();
            setEditorKit(kit);
            javax.swing.text.html.StyleSheet styleSheet = kit.getStyleSheet();
            styleSheet.addRule("body {font-size:" + fontSize + "pt; font-family:" + fontName + ";}");
            Document doc = kit.createDefaultDocument();
            setDocument(doc);

            setPage(new URL(currentHref));
            loadedOkAtConstruction = true;
        } catch (MalformedURLException e) {
            MessageManager.INSTANCE.addMessage(new Message(e.getClass().getCanonicalName() + " " + e.getMessage()));         
        } catch (IOException e) {
            MessageManager.INSTANCE.addMessage(new Message(e.getClass().getCanonicalName() + " " + e.getMessage()));         
        } catch (Exception ex) { 
            Message message = new Message("Cannot load page: " + currentHref + " " + ex.getMessage(), true);
            MessageManager.INSTANCE.addMessage(message);
        } finally {
            if (mainFrame != null) {
                mainFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
        }
    }

    public static String getLoadingMessage(String href, String loadingMessage) {
        return href + HelpContentsPanel.SPACER + loadingMessage + "...";
    }

    /**
     * This internal method attempts to load and display the specified URL. 
     **/
    public boolean visit(String newHref) {
        try {
            if (!newHref.equals(currentHref)) {
                setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
                Message message = new Message(getLoadingMessage(newHref, loadingMessage));
                message.setShowInMessagesTab(false);
                MessageManager.INSTANCE.addMessage(message);
                // Load and display the URL.
                getUrlContentInBackground(this, new URL(newHref));
                currentHref = newHref;
                
                // Remember the new helpContext.
                int index = newHref.indexOf(HelpContentsPanel.HELP_BASE_URL);
                if (index > -1) {
                    String helpContext = newHref.substring(index + HelpContentsPanel.HELP_BASE_URL.length());
                    mainFrame.setHelpContext(helpContext);
                }
            }
           return true; // Return success.
        } catch (IOException ex) { 
            Message message = new Message("Cannot load page: " + ex.getMessage(), true);
            MessageManager.INSTANCE.addMessage(message);
            return false; // Return failure.
        } finally {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));            
        }
    }
    
    /**
     * Get the URL contents in a background thread.
     */
    private void getUrlContentInBackground(final Browser browser, final URL url) {
        SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
            
            private String message = null;
            
            private StringBuffer sb = new StringBuffer();
            
            @Override
            protected Boolean doInBackground() throws Exception {
                try {
                    browser.setLoading(true);
                    
                    InputStream in = url.openStream();

                    byte [] buffer = new byte[256];

                    while(true){
                        int byteRead = in.read(buffer);
                        if(byteRead == -1)
                            break;
                        for(int i = 0; i < byteRead; i++){
                            sb.append((char)buffer[i]);
                        }
                    }
                    return true;
                } catch (IOException ioe) {
                    message = ioe.getClass().getCanonicalName() + " " + ioe.getMessage();
                    return false;
                } finally {
                    browser.setLoading(false);                    
                }
            }
            
            protected void done() {
                Boolean wasSuccessful = false;
                try {
                    wasSuccessful = get();
                    
                    if (wasSuccessful) {
                        browser.setText(sb.toString());
                        // Scroll to top.
                        browser.setCaretPosition(0);
                        MessageManager.INSTANCE.addMessage(new Message(" "));
                    } else {
                        MessageManager.INSTANCE.addMessage(new Message(message));
                    } 
                    
                } catch (InterruptedException e) {
                    message = e.getClass().getCanonicalName() + " " + e.getMessage();
                    e.printStackTrace();
                } catch (ExecutionException e) {
                    message = e.getClass().getCanonicalName() + " " + e.getMessage();
                                               e.printStackTrace();
                } finally {
                    setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                }
            }
        };
        worker.execute();
    }

    public void setLoading(boolean loading) {
        this.loading = loading;
    }

    public boolean isLoading() {
        return loading;
    }

    public boolean wasLoadedOkAtConstruction() {
        return loadedOkAtConstruction;
    }
}
