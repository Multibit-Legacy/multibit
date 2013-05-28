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
import java.util.Collection;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.text.DefaultCaret;

import org.multibit.controller.Controller;
import org.multibit.message.LimitLinesDocumentListener;
import org.multibit.message.Message;
import org.multibit.message.MessageListener;
import org.multibit.message.MessageManager;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;

/**
 * The messages view.
 */
public class MessagesPanel extends JPanel implements Viewable, MessageListener {
    private static final long serialVersionUID = 191662512399957705L;

    private Controller controller;

    private JTextArea textArea;
    
    private String lastMessageAdded = "";
       
  /**
     * Creates a new {@link MessagesPanel}.
     */
    public MessagesPanel(Controller controller, MultiBitFrame mainFrame) {        
         setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        this.controller = controller;
       
        setLayout(new BorderLayout());
        
        textArea = new JTextArea();
        textArea.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setEditable(false);
        
        textArea.getDocument().addDocumentListener(new LimitLinesDocumentListener(MessageManager.INSTANCE.MAXIMUM_NUMBER_OF_MESSAGES_STORED + 1) );
        
        DefaultCaret caret = (DefaultCaret)textArea.getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
        
        JScrollPane mainScrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
        mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

        add(mainScrollPane, BorderLayout.CENTER);

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        
        MessageManager.INSTANCE.addMessageListener(this);
        
        Collection<Message> messages = MessageManager.INSTANCE.getMessages();
        
        for (Message message : messages) {
            if (textArea != null) {
                if (message.getText() != null && !message.getText().equals(lastMessageAdded)) {
                    textArea.append(message.getText() + "\n");
                    lastMessageAdded = message.getText();
                }
            }
        }
    }

    @Override
    public void navigateAwayFromView() {
    }

    @Override
    public void displayView(DisplayHint displayHint) {
    }
       
    @Override
    public Icon getViewIcon() {
        return ImageLoader.createImageIcon(ImageLoader.MESSAGES_ICON_FILE);
    }

    @Override
    public String getViewTitle() {
        return controller.getLocaliser().getString("messagesPanel.text");
    }
    
    @Override
    public String getViewTooltip() {
        return controller.getLocaliser().getString("messagesPanel.tooltip");
    }

    @Override
    public View getViewId() {
        return View.MESSAGES_VIEW;
    }

    @Override
    public void newMessageReceived(final Message message) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (textArea != null) {
                    if (message != null && message.isShowInMessagesTab() && message.getText() != null && !message.getText().equals(lastMessageAdded)) {
                        textArea.append(message.getText() + "\n");
                        lastMessageAdded = message.getText();
                    }                
                }
            }
        });    
    }
}