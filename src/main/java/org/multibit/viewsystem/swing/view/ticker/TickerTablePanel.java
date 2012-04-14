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
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.joda.money.BigMoney;
import org.multibit.controller.MultiBitController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitLabel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.xeiam.xchange.Exchange;
import com.xeiam.xchange.ExchangeFactory;
import com.xeiam.xchange.SymbolPair;
import com.xeiam.xchange.service.marketdata.MarketDataService;
import com.xeiam.xchange.service.marketdata.Ticker;

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

    private static final int VERTICAL_DELTA_MAC = 8;
    private static final int VERTICAL_DELTA_NON_MAC = 16;
    private static final int HORIZONTAL_DELTA = 30;
    private static final int SCROLLBAR_WIDTH = 20;

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
        MouseListener viewPreferencesMouseListener = new MouseAdapter() {
            @Override
            public void mouseReleased(MouseEvent arg0) {
                controller.displayView(View.PREFERENCES_VIEW);
            }
        };

        String tickerTooltipText = HelpContentsPanel.createMultilineTooltipText(new String[] {
                controller.getLocaliser().getString("tickerTablePanel.tooltip"),
                controller.getLocaliser().getString("tickerTablePanel.tooltip.clickToConfigure") });

        addMouseListener(viewPreferencesMouseListener);

        GridBagConstraints constraints = new GridBagConstraints();

        tickerTableModel = new TickerTableModel(controller);
        table = new JTable(tickerTableModel);
        table.setOpaque(true);
        table.setShowGrid(true);
        table.setGridColor(Color.lightGray);

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
        table.getTableHeader().setBorder(BorderFactory.createMatteBorder(0, 1, 0, 0, Color.LIGHT_GRAY));
        table.getTableHeader().setFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-1));

        table.setBorder(BorderFactory.createEmptyBorder());

        TableCellRenderer renderer = table.getTableHeader().getDefaultRenderer();
        JLabel label = (JLabel) renderer;
        label.setHorizontalAlignment(JLabel.CENTER);

        // column widths
        String[] columnVariables = tickerTableModel.getColumnVariables();

        int tickerWidth = HORIZONTAL_DELTA + SCROLLBAR_WIDTH;
        for (int i = 0; i < Math.min(table.getColumnCount(), columnVariables.length); i++) {
            // work out width
            int columnWidth;

            if (TickerTableModel.TICKER_COLUMN_CURRENCY.equals(columnVariables[i])) {
                columnWidth = 4 + Math.max(
                        fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel." + columnVariables[i])),
                        fontMetrics.stringWidth("XYZ")); // currency codes
                                                         // always 3 character
            } else {
                columnWidth = Math.max(
                        fontMetrics.stringWidth(controller.getLocaliser().getString("tickerTableModel." + columnVariables[i])),
                        fontMetrics.stringWidth("0000.00000"));
            }
            tickerWidth += columnWidth;
            table.getColumnModel().getColumn(i).setPreferredWidth(columnWidth);

            if (TickerTableModel.TICKER_COLUMN_CURRENCY.equals(columnVariables[i])) {
                table.getColumnModel().getColumn(i).setCellRenderer(new CurrencyLeadingJustifiedRenderer());
            } else {
                table.getColumnModel().getColumn(i).setCellRenderer(new TrailingJustifiedRenderer());
            }
        }

        int verticalDelta;
        if (mainFrame.getApplication().isMac()) {
            verticalDelta = VERTICAL_DELTA_MAC;
        } else {
            verticalDelta = VERTICAL_DELTA_NON_MAC;
        }

        setPreferredSize(new Dimension(tickerWidth, (2 + fontMetrics.getHeight()) * (tickerTableModel.getRowCount() + 1)
                + verticalDelta));

        JScrollPane scrollPane = new JScrollPane(table, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        scrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        scrollPane.getViewport().setPreferredSize(
                new Dimension(tickerWidth - 2, (2 + fontMetrics.getHeight()) * (tickerTableModel.getRowCount() + 1) + verticalDelta
                        - 2));
        scrollPane.setMinimumSize(new Dimension(tickerWidth, (2 + fontMetrics.getHeight()) * (tickerTableModel.getRowCount() + 1)
                + verticalDelta));

        scrollPane.setComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

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

        mainFrame.getHeaderPanel().invalidate();
        mainFrame.getHeaderPanel().validate();
        mainFrame.getHeaderPanel().repaint();

    }

    class TrailingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row,
                int column) {
            label.setHorizontalAlignment(SwingConstants.TRAILING);
            label.setOpaque(true);
            label.setFont(font);

            String text = "";
            if (value != null) {
                if (value instanceof BigMoney) {
                    text = ((BigMoney) value).getAmount().toPlainString();
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

    class CurrencyLeadingJustifiedRenderer extends DefaultTableCellRenderer {
        private static final long serialVersionUID = 1549545L;

        MultiBitLabel label = new MultiBitLabel("");

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
}