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
package org.multibit.viewsystem.swing.view.ticker;

import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.SystemColor;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A panel with a table showing the exchange rate data
 * 
 * @author jim
 * 
 */
public class TickerTablePanel extends JPanel {

    private static final Logger log = LoggerFactory.getLogger(TickerTablePanel.class);

    private static final long serialVersionUID = 1235108820207842662L;

    private MultiBitController controller;
    private MultiBitFrame mainFrame;

    private JTable table;
    private TickerTableModel tickerTableModel;

    private static final String SPACER = "  "; // 2 spaces

    private static final int VERTICAL_DELTA = 12;
    private static final int HORIZONTAL_DELTA = 30;

    FontMetrics fontMetrics;
    Font font;

    public TickerTablePanel(MultiBitFrame mainFrame, MultiBitController controller) {
        this.controller = controller;
        this.mainFrame = mainFrame;

        font = FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-2);
        fontMetrics = getFontMetrics(font);

        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    private void initUI() {
        createTicker();
    }

    private void createTicker() {
        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        setLayout(new GridBagLayout());
        setOpaque(true);
        setFocusable(false);

        setToolTipText(HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("tickerTablePanel.tooltip"),
                controller.getLocaliser().getString("tickerTablePanel.tooltip.clickToConfigure") }));

        // on mouse click - view the exchanges tab
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent arg0) {
                controller.displayView(View.PREFERENCES_VIEW);
            }
        });

        GridBagConstraints constraints = new GridBagConstraints();

        tickerTableModel = new TickerTableModel(controller);
        table = new JTable(tickerTableModel);
        table.setOpaque(true);
        table.setShowGrid(true);
        //table.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
        table.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        table.setRowHeight(getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight());

        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(false);
        table.setColumnSelectionAllowed(false);
        table.setToolTipText(HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("tickerTablePanel.tooltip"),
                controller.getLocaliser().getString("tickerTablePanel.tooltip.clickToConfigure") }));
        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent arg0) {
                controller.displayView(View.PREFERENCES_VIEW);
            }
        });

        // justify column headers
        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);
        table.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-2));

        // symbol leading justified
        table.getColumnModel().getColumn(0).setCellRenderer(new LeadingJustifiedRenderer());

        TableColumn tableColumn = table.getColumnModel().getColumn(0); // symbol
        int symbolWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.symbol")),
                fontMetrics.stringWidth("iii USD"));
        tableColumn.setPreferredWidth(symbolWidth);

        int rateWidth = 0;
        int bidWidth = 0;
        int askWidth = 0;
        int exchangeWidth = 0;
        
        if (tickerTableModel.isShowRate()) {
            // rate
            table.getColumnModel().getColumn(1).setCellRenderer(new TrailingJustifiedRenderer());
            rateWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.bid")),
                    fontMetrics.stringWidth("000.0000"));
            table.getColumnModel().getColumn(1).setPreferredWidth(rateWidth);

            // exchange
            table.getColumnModel().getColumn(2).setCellRenderer(new TrailingJustifiedRenderer());
            exchangeWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.exchange")),
                    fontMetrics.stringWidth("Mt.Gox"));
            table.getColumnModel().getColumn(2).setPreferredWidth(exchangeWidth);
        } else {
             // bid
            table.getColumnModel().getColumn(1).setCellRenderer(new TrailingJustifiedRenderer());
            bidWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.bid")),
                    fontMetrics.stringWidth("000.0000"));
            table.getColumnModel().getColumn(1).setPreferredWidth(bidWidth);

            // ask
            table.getColumnModel().getColumn(2).setCellRenderer(new TrailingJustifiedRenderer());
            askWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.ask")),
                    fontMetrics.stringWidth("000.0000"));
            table.getColumnModel().getColumn(2).setPreferredWidth(askWidth);
            
            // exchange
            table.getColumnModel().getColumn(3).setCellRenderer(new TrailingJustifiedRenderer());
            exchangeWidth = Math.max(fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel.exchange")),
                    fontMetrics.stringWidth("Mt.Gox"));
            table.getColumnModel().getColumn(3).setPreferredWidth(exchangeWidth);
         }

        int overallWidth = symbolWidth + rateWidth + bidWidth + askWidth + exchangeWidth + HORIZONTAL_DELTA;

        setPreferredSize(new Dimension(overallWidth, fontMetrics.getHeight() * (tickerTableModel.getRowCount() + 1) + VERTICAL_DELTA));

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.getViewport()
                .setPreferredSize(new Dimension(overallWidth - 2, fontMetrics.getHeight() * (tickerTableModel.getRowCount() + 1) + VERTICAL_DELTA - 2));
        scrollPane.setMinimumSize(new Dimension(overallWidth, fontMetrics.getHeight() * (tickerTableModel.getRowCount() + 1) + VERTICAL_DELTA));

        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;

        add(scrollPane, constraints);
    }

    public void update() {
        table.invalidate();
        table.validate();
        table.repaint();

        invalidate();
        validate();
        repaint();
    }

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setFont(font);
            label.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1,Color.LIGHT_GRAY));

            label.setText(value + SPACER);

            Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                    : ColorAndFontConstants.BACKGROUND_COLOR);
            label.setBackground(backgroundColor);
            label.setForeground(table.getForeground());

            return label;
        }
    }

    class LeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setText(SPACER + (String) value);
            label.setFont(font);
            label.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.LIGHT_GRAY));


            Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                    : ColorAndFontConstants.BACKGROUND_COLOR);
            label.setBackground(backgroundColor);
            label.setForeground(table.getForeground());

            return label;
        }
    }
}