package org.multibit.viewsystem.swing;

import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.view.components.FontSizer;

public class MultiBitTabbedPane extends JTabbedPane {

    private static final long serialVersionUID = 6530125716859367873L;

    private ImageIcon closeTabIcon;
    private Dimension closeButtonSize;

    private int tabCounter = 0;

    private MultiBitController controller;

    private final MultiBitTabbedPane thisTabbedPane;
    
    private static boolean enableUpdates = false;

    public MultiBitTabbedPane(final MultiBitController controller) {
        thisTabbedPane = this;
        this.controller = controller;
        
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        // Create an image icon of the small 'X' for use with a close
        // button on each tab. The png loaded is a 10x10 graphic
        closeTabIcon = ImageLoader.createImageIcon(ImageLoader.CLOSE_TAB_ICON_FILE);

        // Create a Dimension that can be used to size the close buttons.
        closeButtonSize = new Dimension(closeTabIcon.getIconWidth() + 2, closeTabIcon.getIconHeight() + 2);
    }
    
    public void setSelectedIndex(int index) {
        super.setSelectedIndex(index);
        
        System.out.println("MultiBitTabbedPane. Selecting tab '" + index + "', enableUpdates = " + enableUpdates );
        
        if (!enableUpdates) {
            return;
        }
        
        // Get current tab.
        JPanel tabComponent = (JPanel) getComponentAt(index);
        if (tabComponent != null) {
            Component[] childComponents = tabComponent.getComponents();
            View selectedView = null;
            if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof View) {
                selectedView = ((View) childComponents[0]);
                if (selectedView != null && controller.getCurrentView() == selectedView.getViewId()) {
                    // We are already displaying the correct tab.
                    // Just update the contents.
                    selectedView.displayView();
                    controller.fireDataChanged();
                } else {
                    // Select the new tab, update the content.
                    controller.setCurrentView(selectedView.getViewId());
                    selectedView.displayView();
                    controller.fireDataChanged();
                }
            }
        }
    }
    
    public void refreshShownTab() {
        // Get current tab.
        JPanel tabComponent = (JPanel) getSelectedComponent();
        if (tabComponent != null) {
            Component[] childComponents = tabComponent.getComponents();
            View selectedView = null;
            if (childComponents != null && childComponents.length > 0 && childComponents[0] instanceof View) {
                selectedView = ((View) childComponents[0]);
                if (selectedView != null) {
                    selectedView.displayView();
                } 
            }
        }
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
                public void actionPerformed(ActionEvent e) {
                    int closeTabNumber = thisTabbedPane.indexOfComponent(finalComponent);

                    thisTabbedPane.removeTabAt(closeTabNumber);

                    // notify controller of new view being shown
                    JPanel selectedTab = (JPanel) thisTabbedPane.getSelectedComponent();
                    Component[] components = selectedTab.getComponents();
                    if (components != null && components.length > 0 && components[0] instanceof View) {
                        controller.displayView(((View) components[0]).getViewId());
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
        setTabComponentAt(getTabCount() - 1, tab);
        //tab.setToolTipText(tooltip);
    }

    public static boolean isEnableUpdates() {
        return enableUpdates;
    }

    public static void setEnableUpdates(boolean enableUpdates) {
        MultiBitTabbedPane.enableUpdates = enableUpdates;
    }
}
