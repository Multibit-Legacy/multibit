package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.browser.Browser;

public class HelpContentsPanel extends JPanel implements View {

    private static final long serialVersionUID = 4921443778446348403L;

    private Browser browser;
    private String initialUrl;

    private MultiBitFrame mainFrame;

    public static final String SPACER = "   "; // 3 spaces

    boolean firstTimeLoaded = false;

    public HelpContentsPanel(MultiBitController controller, MultiBitFrame mainFrame) {
        this.mainFrame = mainFrame;
        this.initialUrl = "http://www.multibit.org/help_contents.html";

        setLayout(new BorderLayout());
        firstTimeLoaded = true;

        setBorder(BorderFactory.createEmptyBorder(0, 0, 1, 0));
        setBackground(MultiBitFrame.BACKGROUND_COLOR);

        browser = new Browser(controller, mainFrame, initialUrl);
 
        JScrollPane scrollPane = new JScrollPane(browser);
        scrollPane.setPreferredSize(new Dimension(800, 400));
        add(scrollPane, BorderLayout.CENTER);
    }

    public void displayView() {
        if (!firstTimeLoaded) {
            if (browser != null) {
                browser.visit(initialUrl);
            }
        }
        firstTimeLoaded = false;
    }

    public void navigateAwayFromView(int nextViewId, int relationshipOfNewViewToPrevious) {
        mainFrame.updateStatusLabel("");
    }

    public void displayMessage(String messageKey, Object[] messageData, String titleKey) {
    }
    

    public String getDescription() {
        return null;
    }
}