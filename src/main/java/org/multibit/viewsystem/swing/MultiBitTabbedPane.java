package org.multibit.viewsystem.swing;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import org.multibit.utils.ImageLoader;

public class MultiBitTabbedPane extends JTabbedPane {

    private static final long serialVersionUID = 6530125716859367873L;

    private ImageIcon closeTabIcon;
    private Dimension closeButtonSize;

    private int tabCounter = 0;

    private final MultiBitTabbedPane thisTabbedPane;

    public MultiBitTabbedPane() {
        thisTabbedPane = this;



        // Create an image icon of the small 'X' for use with a close
        // button on each tab. The png loaded is a 10x10 graphic
        closeTabIcon = ImageLoader.createImageIcon(ImageLoader.CLOSE_TAB_ICON_FILE);

        // Create a Dimension that can be used to size the close buttons.
        closeButtonSize = new Dimension(closeTabIcon.getIconWidth() + 2, closeTabIcon.getIconHeight() + 2);
    }

    public void addUncloseableTab(String title, Icon icon, Component component) {
        super.addTab(title, icon, component);
    }

    @Override
    public void addTab(String title, Icon icon, Component component) {
        final Component finalComponent = component;

        // Create a panel that represents the tab and ensure that it is
        // transparent.
        JPanel tab = new JPanel(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();

        tab.setOpaque(false);

        // Create a label and a Close button for the tab. Be sure to
        // set its preferred size to nearly the size of the icon, and
        // create an action listener that will locate the tab and
        // remote it from the tabbed pane.

        JLabel tabLabel = new JLabel(title);
        tabLabel.setIcon(icon);
        tabCounter++;

        JButton tabCloseButton = new JButton(closeTabIcon);
        tabCloseButton.setPreferredSize(closeButtonSize);
        tabCloseButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int closeTabNumber = thisTabbedPane.indexOfComponent(finalComponent);
                thisTabbedPane.removeTabAt(closeTabNumber);
            }
        });

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.8;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        tab.add(tabLabel, constraints);

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

        // Add the tab to the tabbed pane. Note that the first
        // parameter, which would ordinarily be a String that
        // represents the tab title, is null.
        addTab(null, component);

        // Instead of using a String/Icon combination for the tab,
        // use our panel instead.
        setTabComponentAt(getTabCount() - 1, tab);
    }
}
