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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import org.joda.money.BigMoney;
import org.multibit.controller.Controller;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;

/**
 * A panel with a table showing the exchange rate data.
 * 
 * @author jim
 * 
 */
public class TickerTablePanel extends JPanel {
    private static final long serialVersionUID = 1235108820207842662L;

    private final Controller controller;
    private final ExchangeController exchangeController;
    
    private MultiBitFrame mainFrame;

    private JTable table;
    private TickerTableModel tickerTableModel;
    private JScrollPane scrollPane;

    private static final String SPACER = "  "; // 2 spaces

    private static final int HORIZONTAL_DELTA = 30;
    private static final int SCROLLBAR_WIDTH = 20;
    private static final int PER_COLUMN_DELTA = 6;
    
    FontMetrics fontMetrics;
    Font font;
    
    private int idealHeight;

    public TickerTablePanel(MultiBitFrame mainFrame, ExchangeController exchangeController) {
        this.exchangeController = exchangeController;
        this.controller = this.exchangeController;
        
        this.mainFrame = mainFrame;

        font = FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-1);
        fontMetrics = getFontMetrics(font);
        
        initUI();

        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    }

    private void initUI() {
        createTicker();
    }

    private void createTicker() {       
        setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        setLayout(new GridBagLayout());
        setOpaque(true);
        setFocusable(false);

        setToolTipText(HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("tickerTablePanel.tooltip"), "\n ",
                controller.getLocaliser().getString("tickerTablePanel.tooltip.clickToConfigure") }));

        // on mouse click - view the exchanges tab
        MouseListener viewPreferencesMouseListener = new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent arg0) {
                controller.displayView(View.PREFERENCES_VIEW);
            }
        };

        String tickerTooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("tickerTablePanel.tooltip"), "\n ",
                controller.getLocaliser().getString("tickerTablePanel.tooltip.clickToConfigure") });

        addMouseListener(viewPreferencesMouseListener);

        GridBagConstraints constraints = new GridBagConstraints();

        tickerTableModel = new TickerTableModel(this.exchangeController);
        
        table = new JTable(tickerTableModel);
        table.setOpaque(true);
        table.setShowGrid(true);
        table.setGridColor(Color.lightGray);
        table.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        
        table.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        table.setRowHeight(getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight());

        table.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        table.setRowSelectionAllowed(false);
        table.setColumnSelectionAllowed(false);
        table.getTableHeader().setReorderingAllowed(false);

        table.setToolTipText(tickerTooltipText);
        table.addMouseListener(viewPreferencesMouseListener);
        table.getTableHeader().addMouseListener(viewPreferencesMouseListener);
        table.getTableHeader().setToolTipText(tickerTooltipText);
        table.getTableHeader().setBorder(BorderFactory.createMatteBorder(1, 1, 0, 0, Color.LIGHT_GRAY));
        table.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-1));
        
        int tableHeaderVerticalInsets = table.getTableHeader().getInsets().top + table.getTableHeader().getInsets().bottom;

        int tickerWidth = setupColumnWidths();
        setupTableHeaders();

        idealHeight =  (fontMetrics.getHeight() + table.getRowMargin()) * tickerTableModel.getRowCount() 
            + fontMetrics.getHeight() + tableHeaderVerticalInsets + tickerTableModel.getRowCount() + 8;


        setPreferredSize(new Dimension(tickerWidth, idealHeight));

        scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.addMouseListener(viewPreferencesMouseListener);
 
        setupScrollPane(tickerWidth);

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;

        add(scrollPane, constraints);
    }
    
    private int setupColumnWidths() {
        // Column widths.
        String[] columnVariables = tickerTableModel.getColumnVariables();

        int tickerWidth = HORIZONTAL_DELTA;
        if (tickerTableModel.getRowCount() > 1) {
            // There may be a scroll bar so give it some space.
            tickerWidth += SCROLLBAR_WIDTH;
        }
        
        int numberOfColumns = Math.min(table.getColumnCount(), columnVariables.length);
        for (int i = 0; i < numberOfColumns; i++) {
            // work out width
            int columnWidth;

            if (TickerTableModel.TICKER_COLUMN_CURRENCY.equals(columnVariables[i])) {
                columnWidth = PER_COLUMN_DELTA + Math.max(Math.max(
                        fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel." + columnVariables[i])),
                        fontMetrics.stringWidth((String)tickerTableModel.getValueAt(0, i))), 
                        fontMetrics.stringWidth((String)tickerTableModel.getValueAt(1, i))); 
            } else if (TickerTableModel.TICKER_COLUMN_EXCHANGE.equals(columnVariables[i])) {
                columnWidth = PER_COLUMN_DELTA + Math.max(Math.max(
                        fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel." + columnVariables[i])),
                        fontMetrics.stringWidth((String)tickerTableModel.getValueAt(0, i))), 
                        fontMetrics.stringWidth((String)tickerTableModel.getValueAt(1, i))); 
            } else {
                columnWidth = PER_COLUMN_DELTA + Math.max(
                        fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel." + columnVariables[i])),
                        fontMetrics.stringWidth("000000.00000"));
            }
            tickerWidth += columnWidth;
            table.getColumnModel().getColumn(i).setPreferredWidth(columnWidth);
            
        }
        return tickerWidth;
    }
    
    private void setupTableHeaders() {
        // Column justification.
        String[] columnVariables = tickerTableModel.getColumnVariables();

        int numberOfColumns = Math.min(table.getColumnCount(), columnVariables.length);
        for (int i = 0; i < numberOfColumns; i++) {
            TableCellRenderer columnRenderer;
            if (i == numberOfColumns - 1) {
                columnRenderer = new CurrencyCenterJustifiedWithRightBorderRenderer();
            } else {
                columnRenderer = new CurrencyCenterJustifiedRenderer();
            }
            table.getColumnModel().getColumn(i).setCellRenderer(columnRenderer);
        }

        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);
    }
    
    private void setupScrollPane(int tickerWidth) {
        scrollPane.getViewport().setPreferredSize(
                new Dimension(tickerWidth, idealHeight));
        scrollPane.setMinimumSize(new Dimension(tickerWidth, Math.min(idealHeight, MultiBitFrame.HEIGHT_OF_HEADER)));

        scrollPane.setOpaque(false);
        scrollPane.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.getViewport().setOpaque(false);
        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    }

    public void update() {
        int tickerWidth = setupColumnWidths();

        setupTableHeaders();
        setupScrollPane(tickerWidth);
        table.invalidate();
        table.validate();
        table.repaint();

        invalidate();
        validate();
        repaint();

        mainFrame.getHeaderPanel().invalidate();
        mainFrame.getHeaderPanel().validate();
        mainFrame.getHeaderPanel().repaint();
    }

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setFont(font);

            String text = "";
            if (value != null) {
                if (value instanceof BigMoney) {
                    //text = ((BigMoney) value).getAmount().toPlainString();
                    text = controller.getLocaliser().bigMoneyValueToString(((BigMoney) value));
                } else {
                    text = value.toString();
                }
            }
            label.setText(text + SPACER);

            if (column == 0) {
                label.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.LIGHT_GRAY));
            }

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

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.LEADING);
            label.setOpaque(true);
            label.setFont(font);

            String text = "";
            if (value != null) {
                if (value instanceof BigMoney) {
                    //text = ((BigMoney) value).getAmount().toPlainString();
                    text = controller.getLocaliser().bigMoneyValueToString(((BigMoney) value));
                } else {
                    text = value.toString();
                }
            }
            label.setText(SPACER + text);

            if (column == 0) {
                label.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.LIGHT_GRAY));
            }

            Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                    : ColorAndFontConstants.BACKGROUND_COLOR);
            label.setBackground(backgroundColor);
            label.setForeground(table.getForeground());

            return label;
        }
    }

    class CurrencyCenterJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setText((String) value);
            label.setFont(font);
            label.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.LIGHT_GRAY));

            Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                    : ColorAndFontConstants.BACKGROUND_COLOR);
            label.setBackground(backgroundColor);
            label.setForeground(table.getForeground());

            return label;
        }
    }
       
    class CurrencyCenterJustifiedWithRightBorderRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 9949545L;

        MultiBitLabel label = new MultiBitLabel("");

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.CENTER);
            label.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
            label.setOpaque(true);
            label.setText((String) value);
            label.setFont(font);
            label.setBorder(BorderFactory.createMatteBorder(0, 1, 0, 1, Color.LIGHT_GRAY));

            Color backgroundColor = (row % 2 == 0 ? ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR
                    : ColorAndFontConstants.BACKGROUND_COLOR);
            label.setBackground(backgroundColor);
            label.setForeground(table.getForeground());

            return label;
        }
    }
}