package org.multibit.viewsystem.swing.view.components;

import org.multibit.Localiser;
import org.multibit.viewsystem.swing.ColorAndFontConstants;

import javax.swing.*;
import java.awt.*;

/**
 * Panel that has a title bar at the top. The panel always uses GridBagLayout
 * with the title in row 0.
 * 
 * @author jim
 * 
 */
public class MultiBitTitledPanel extends JPanel {
	
    private static final long serialVersionUID = 4532881356315823376L;

    public static final int LEFT_INDENT = 25;

    public static final int STENT_DELTA = 16;
    
    public static final int SEPARATION_BETWEEN_NAME_VALUE_PAIRS = 16;

    public static final int MAXIMUM_NUMBER_OF_COLUMNS = 10;

    public MultiBitTitledPanel(String title, ComponentOrientation componentOrientation) {
        setLayout(new GridBagLayout());
        setOpaque(false);
        setComponentOrientation(componentOrientation);

        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();
        setFont(font);

        FontMetrics fontMetrics = getFontMetrics(font);

        int preferredHeight = (int)(fontMetrics.getHeight() * 1.618);
        int spacerHeight = (int)(fontMetrics.getHeight() * 0.618);

        HorizontalGradientPanel titlePanel = new HorizontalGradientPanel(componentOrientation);
        titlePanel.setLayout(new GridBagLayout());
        titlePanel.setComponentOrientation(componentOrientation);

        Font titleFont = FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(ColorAndFontConstants.MULTIBIT_LARGE_FONT_INCREASE);

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.05;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        titlePanel.add(MultiBitTitledPanel.getIndentPanel(preferredHeight), constraints);

        constraints.fill = GridBagConstraints.NONE;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.95;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        MultiBitLabel titleLabel = new MultiBitLabel(title, JLabel.LEFT);
        titleLabel.setFont(titleFont);

        titlePanel.add(titleLabel, constraints);

        JPanel fill1 = new JPanel();
        fill1.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.weightx = 1000;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        titlePanel.add(fill1, constraints);

        GridBagConstraints constraints2 = new GridBagConstraints();
        
        // Put a half height space above the title.
        JPanel spacer = new JPanel();
        spacer.setOpaque(false);
        spacer.setMinimumSize(new Dimension(1, spacerHeight));
        spacer.setPreferredSize(new Dimension(1, spacerHeight));
        spacer.setMaximumSize(new Dimension(1, spacerHeight));
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 0;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.weightx = 0.01;
        constraints2.weighty = 0.01;
        constraints2.anchor = GridBagConstraints.CENTER;
        add(spacer, constraints2);
        
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 1;
        constraints2.gridwidth = MAXIMUM_NUMBER_OF_COLUMNS;
        constraints2.gridheight = 1;
        constraints2.weightx = 1;
        constraints2.weighty = 0.1;
        constraints2.anchor = GridBagConstraints.ABOVE_BASELINE_LEADING;
        // Put the title in the second row.
        add(titlePanel, constraints2);

        // Put a half height space under the title.
        JPanel spacer2 = new JPanel();
        spacer2.setOpaque(false);
        spacer2.setMinimumSize(new Dimension(1, spacerHeight));
        spacer2.setPreferredSize(new Dimension(1, spacerHeight));
        spacer2.setMaximumSize(new Dimension(1, spacerHeight));
        constraints2.fill = GridBagConstraints.BOTH;
        constraints2.gridx = 0;
        constraints2.gridy = 2;
        constraints2.gridwidth = 1;
        constraints2.gridheight = 1;
        constraints2.weightx = 0.01;
        constraints2.weighty = 0.01;
        constraints2.anchor = GridBagConstraints.CENTER;
        add(spacer2, constraints2);
    }

    public static JPanel getIndentPanel(int height) {
        JPanel filler1 = new JPanel();
        filler1.setOpaque(false);
        filler1.setMinimumSize(new Dimension(LEFT_INDENT, height));
        filler1.setPreferredSize(new Dimension(LEFT_INDENT, height));
        filler1.setMaximumSize(new Dimension(LEFT_INDENT, height));

        return filler1;
    }

    public static MultiBitLabel addLeftJustifiedTextAtIndent(String text, int row, JPanel panel) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = row;
        constraints.weightx = 0.1;
        constraints.weighty = 0.05;
        constraints.gridwidth = 1;
        constraints.gridheight = 1;
        constraints.anchor = GridBagConstraints.LINE_START;
        JPanel indent = MultiBitTitledPanel.getIndentPanel(1);
        panel.add(indent, constraints);
 
        MultiBitLabel label = new MultiBitLabel(text);
        label.setBorder(BorderFactory.createEmptyBorder(0, 0, 4, 0));
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = row;
        constraints.weightx = 0.6;
        constraints.weighty = 0.3;
        constraints.gridwidth = 3;
        constraints.anchor = GridBagConstraints.LINE_START;
        panel.add(label, constraints);

        JPanel filler0 = new JPanel();
        filler0.setOpaque(false);
        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = MAXIMUM_NUMBER_OF_COLUMNS - 1;
        constraints.gridy = row;
        constraints.weightx = 100;
        constraints.weighty = 1;
        constraints.gridwidth = 1;
        constraints.anchor = GridBagConstraints.LINE_END;
        panel.add(filler0, constraints);

        return label;
    }

    public static int calculateStentWidthForKeys(Localiser localiser, String[] keys, JPanel targetPanel) {
        Font font = FontSizer.INSTANCE.getAdjustedDefaultFont();

        FontMetrics fontMetrics = targetPanel.getFontMetrics(font);

        int minimumWidth = 0;
        if (keys != null) {
            for (int i = 0; i < keys.length; i++) {
                minimumWidth = Math.max(minimumWidth, fontMetrics.stringWidth(localiser.getString(keys[i])));
            }
        }

        minimumWidth += STENT_DELTA;

        return minimumWidth;
    }

    public static JPanel createStent(int stentWidth) {
        return createStent(stentWidth, 1);
    }
    
    public static JPanel createStent(int stentWidth, int stentHeight) {
        JPanel stentPanel = new JPanel();
        stentPanel.setOpaque(false);
        //stentPanel.setBorder(BorderFactory.createLineBorder(Color.GREEN));

        stentPanel.setMinimumSize(new Dimension(stentWidth, stentHeight));
        stentPanel.setPreferredSize(new Dimension(stentWidth, stentHeight));
        stentPanel.setMaximumSize(new Dimension(stentWidth, stentHeight));
        return stentPanel;
    }
}
