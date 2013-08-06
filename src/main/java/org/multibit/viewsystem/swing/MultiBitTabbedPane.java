package org.multibit.viewsystem.swing;

import org.multibit.controller.Controller;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.plaf.TabbedPaneUI;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

public class MultiBitTabbedPane extends JTabbedPane {

    private static final long serialVersionUID = 6530125716859367873L;

    private ImageIcon closeTabIcon;
    private Dimension closeButtonSize;

    private static final int CLOSE_ICON_WIDTH = 10;
    private static final int CLOSE_ICON_HEIGHT = 10;
    private static final int SEPARATION_DISTANCE = 2;

    private int tabCounter = 0;

    private Controller controller;

    private final MultiBitTabbedPane thisTabbedPane;
    
    private static boolean enableUpdates = false;

    private static final Logger log = LoggerFactory.getLogger(MultiBitTabbedPane.class);

    public MultiBitTabbedPane(final Controller controller) {
        thisTabbedPane = this;
        this.controller = controller;
        
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        // Create an image icon of the small 'X' for use with a close
        // button on each tab. The png loaded is a 10x10 graphic
        closeTabIcon = ImageLoader.createImageIcon(ImageLoader.CLOSE_TAB_ICON_FILE);

        // Create a Dimension that can be used to size the close buttons.
        closeButtonSize = new Dimension(CLOSE_ICON_WIDTH + SEPARATION_DISTANCE, CLOSE_ICON_HEIGHT + SEPARATION_DISTANCE);
        
        ToolTipManager.sharedInstance().registerComponent(this);
    }
    
    @Override
    public void setSelectedIndex(int index) {
        super.setSelectedIndex(index);
        
        if (!enableUpdates) {
            return;
        }

        log.debug("Set selected index = " + index);

        try {
            // Get current tab.
            JPanel tabPanelComponent = (JPanel) getComponentAt(index);
            Viewable selectedView = null;
            if (tabPanelComponent != null) {
                Component[] childComponents = tabPanelComponent.getComponents();
                selectedView = null;
                if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof Viewable) {
                    selectedView = ((Viewable) childComponents[0]);
                    if (selectedView != null && controller.getCurrentView() == selectedView.getViewId()) {
                        // We are already displaying the correct tab.
                        // Just update the contents.
                        selectedView.displayView(DisplayHint.COMPLETE_REDRAW);
                        controller.fireDataChangedUpdateNow();
                    } else {
                        // Select the new tab, update the content.
                        controller.setCurrentView(selectedView.getViewId());
                        selectedView.displayView(DisplayHint.COMPLETE_REDRAW);

                        // Fire data change but no need to redisplay the view
                        enableUpdates = false;
                        controller.fireDataChangedUpdateNow();
                        enableUpdates = true;
                    }
                }
            }

            Component tabComponent = getTabComponentAt(index);
            if (tabComponent != null && tabComponent instanceof JLabel) {
                JLabel tabLabel = (JLabel) tabComponent;
                if (selectedView != null) {
                    tabLabel.setToolTipText(HelpContentsPanel.createTooltipText(selectedView.getViewTooltip()));
                }
            }
        } catch (Throwable e) {
            // Do not let errors percolate out of tab display.
            log.error(e.getClass().getName() + " " + e.getMessage());
        }
    }
    
    public Viewable getCurrentlyShownView() {
        // Get current tab.
        JPanel tabComponent = (JPanel) getSelectedComponent();
        if (tabComponent != null) {
            Component[] childComponents = tabComponent.getComponents();
            Viewable selectedView = null;
            if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof Viewable) {
                selectedView = ((Viewable) childComponents[0]);
                return selectedView;
            }
        }
        return null;
    }
    
    @Override
    public void addTab(String title, Icon icon, Component component) {
        addTab(title, icon, "", component, false);
    }

    public void addTab(String title, Icon icon, String tooltip, Component component) {
        addTab(title, icon, tooltip, component, false);
    }

    public void addTab(String title, Icon icon, String tooltip, Component component, boolean isCloseable) {
        final Component finalComponent = component;
       
        // Create a panel that represents the tab and ensure that it is
        // transparent.
        JPanel tab = new JPanel(new GridBagLayout());

        tab.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        tab.setOpaque(false);

        // Create a label and a Close button for the tab. Be sure to
        // set its preferred size to nearly the size of the icon, and
        // create an action listener that will locate the tab and
        // remote it from the tabbed pane.

        JLabel tabLabel = new JLabel(title);
        tabLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
        tabLabel.setIcon(icon);
        tabLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        tabCounter++;

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        tab.add(tabLabel, constraints);

        if (isCloseable) {
            JButton tabCloseButton = new JButton(closeTabIcon);
            tabCloseButton.setPreferredSize(closeButtonSize);
            tabCloseButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    int closeTabNumber = thisTabbedPane.indexOfComponent(finalComponent);

                    thisTabbedPane.removeTabAt(closeTabNumber);

                    // notify controller of new view being shown
                    JPanel selectedTab = (JPanel) thisTabbedPane.getSelectedComponent();
                    Component[] components = selectedTab.getComponents();
                    if (components != null && components.length > 0 && components[0] instanceof Viewable) {
                        Viewable selectedView = (Viewable) components[0];
                        selectedView.displayView(DisplayHint.COMPLETE_REDRAW);

                        controller.displayView(selectedView.getViewId());
                    }
                }
            });
            
            JPanel fill1 = new JPanel();
            fill1.setOpaque(false);
            fill1.setMinimumSize(new Dimension(4, 4));
            fill1.setPreferredSize(new Dimension(4, 4));
            fill1.setMaximumSize(new Dimension(4, 4));

            constraints.fill = GridBagConstraints.BOTH;
            constraints.gridx = 1;
            constraints.gridy = 0;
            constraints.gridwidth = 1;
            constraints.gridheight = 1;
            constraints.weightx = 0.05;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.CENTER;
            tab.add(fill1, constraints);

            constraints.fill = GridBagConstraints.NONE;
            constraints.gridx = 2;
            constraints.gridy = 0;
            constraints.gridwidth = 1;
            constraints.gridheight = 1;
            constraints.weightx = 0.2;
            constraints.weighty = 1;
            constraints.anchor = GridBagConstraints.BASELINE_TRAILING;
            tab.add(tabCloseButton, constraints);
        }

        // Add the tab to the tabbed pane. Note that the first
        // parameter, which would ordinarily be a String that
        // represents the tab title, is null.
        addTab(null, component);    
        
        // Instead of using a String/Icon combination for the tab,
        // use our panel instead.
        ToolTipManager.sharedInstance().unregisterComponent(tab);
        setTabComponentAt(getTabCount() - 1, tab);
    }
    
    @Override
    public String getToolTipText(MouseEvent e) {
        int index = ((TabbedPaneUI)ui).tabForCoordinate(this, e.getX(), e.getY());

        if (index != -1) {
            JComponent selectedTab = (JComponent)getComponentAt(index);
            Component[] components = selectedTab.getComponents();
            if (components != null && components.length > 0 && components[0] instanceof Viewable) {
                return HelpContentsPanel.createTooltipText(((Viewable) components[0]).getViewTooltip());
            }
        }

        return null;
    }

    public void removeAllTabs() {
        int tabCount = this.getTabCount();
        for (int i = 0; i < tabCount; i++) {
            this.removeTabAt(0);
        }
    }
    
    public static boolean isEnableUpdates() {
        return enableUpdates;
    }

    public static void setEnableUpdates(boolean enableUpdates) {
        MultiBitTabbedPane.enableUpdates = enableUpdates;
    }

    public Insets getInsets() {
        return new Insets(0, 0, 0, 0);
    }
}
