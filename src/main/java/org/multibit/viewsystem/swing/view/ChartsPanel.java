/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.multibit.controller.MultiBitController;
import org.multibit.utils.DateUtils;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;

import com.google.bitcoin.core.Transaction;
import com.xeiam.xchart.Chart;
import com.xeiam.xchart.series.Series;
import com.xeiam.xchart.series.SeriesColor;
import com.xeiam.xchart.swing.XChartJPanel;

/**
 * The Charts view.
 */
public class ChartsPanel extends JPanel implements View, ComponentListener {

    private static final long serialVersionUID = 191352212345998705L;

    private static final int NUMBER_OF_DAYS_TO_LOOK_BACK = 30;
    private static final BigInteger NUMBER_OF_SATOSHI_IN_ONE_BTC = BigInteger.valueOf(100000000);

    private MultiBitController controller;

    private JPanel mainPanel;

    // Formatter for dates.
    private SimpleDateFormat dateFormatter;

    private Chart chart;

    /**
     * Creates a new {@link ChartsPanel}.
     */
    public ChartsPanel(MultiBitController controller, MultiBitFrame mainFrame) {

        this.controller = controller;

        setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

        mainFrame.addComponentListener(this);
        addComponentListener(this);
        
        initUI();
    }

    private void initUI() {

        mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());
        mainPanel.setOpaque(false);
        mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(createChartPanel(), constraints);

        JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mainScrollPane.getViewport().setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
        mainScrollPane.getViewport().setOpaque(true);
        mainScrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        add(mainScrollPane, BorderLayout.CENTER);
    }

    /**
     * Create a panel containing the chart to show.
     */
    private JPanel createChartPanel() {

        Chart chart = new Chart(this.getWidth(), this.getHeight());

        // generates linear data
        Collection<Date> xData = new ArrayList<Date>();
        Collection<Number> yData = new ArrayList<Number>();

        // Get the last month's transaction data.
        Collection<ChartData> chartDataCollection = getChartData();

        if (chartDataCollection == null || chartDataCollection.size() == 0) {
            JPanel panelToReturn = new JPanel();
            panelToReturn.setOpaque(false);
            return panelToReturn;
        }

        System.out.println(chartDataCollection.toString());

        for (ChartData chartData : chartDataCollection) {
            System.out.println(chartData.toString());
            xData.add(chartData.getDate());
            yData.add(chartData.getValue().divide(NUMBER_OF_SATOSHI_IN_ONE_BTC));
        }

        // Customize Chart.
        String xAxisLabel = controller.getLocaliser().getString("walletData.dateText");
        String balanceLabel = controller.getLocaliser().getString("multiBitFrame.balanceLabel");
        String chartTitle = controller.getLocaliser().getString("chartsPanelTitle.text",
                new Object[] { NUMBER_OF_DAYS_TO_LOOK_BACK });

        chart.setChartTitle(chartTitle);
        chart.setXAxisTitle(xAxisLabel);
        chart.setYAxisTitle(balanceLabel);
        chart.setChartGridlinesVisible(false);
        chart.setXAxisTicksVisible(false);
        chart.setXAxisTitleVisible(true);
        chart.setChartLegendVisible(false);
        Series series = chart.addDateSeries(balanceLabel, xData, yData);
        series.setLineColor(SeriesColor.BLUE);
        series.setMarkerColor(SeriesColor.BLUE);

        XChartJPanel chartPanelToReturn = new XChartJPanel(chart);
        chartPanelToReturn.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
        chartPanelToReturn.setOpaque(false);
        return chartPanelToReturn;
    }

    /**
     * Update the chart panel (The active wallet may have changed).
     */
    private void updateChart() {

        // Clear the main panel.
        mainPanel.removeAll();

        // Recreate the chart data and 'draw' it.
        GridBagConstraints constraints = new GridBagConstraints();

        constraints.fill = GridBagConstraints.BOTH;
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 1;
        constraints.weighty = 1;
        constraints.anchor = GridBagConstraints.CENTER;
        mainPanel.add(createChartPanel(), constraints);
    }

    /**
     * Get the transaction data for the chart
     */
    private Collection<ChartData> getChartData() {

        if (controller.getModel().getActiveWallet() == null) {
            return new ArrayList<ChartData>();
        }

        ArrayList<Transaction> allTransactions = new ArrayList<Transaction>(controller.getModel().getActiveWallet()
                .getTransactions(false, false));

        // Order by date.
        Collections.sort(allTransactions, new Comparator<Transaction>() {

            @Override
            public int compare(Transaction t1, Transaction t2) {

                return t1.getUpdateTime().compareTo(t2.getUpdateTime());
            }
        });

        // Work out balance as running total and filter to just last
        // NUMBER_OF_DAYS_TO_LOOK_BACKs data.
        BigInteger balance = BigInteger.ZERO;

        // The previous datums balance.
        BigInteger previousBalance = BigInteger.ZERO;

        // The previous datums timepoint.
        Date previousDate = null;

        long pastInMillis = DateUtils.nowUtc().plusDays(-1 * NUMBER_OF_DAYS_TO_LOOK_BACK).getMillis();

        // Create ChartData collection.
        Collection<ChartData> chartData = new ArrayList<ChartData>();

        try {

            boolean leftEdgeDataPointAdded = false;

            for (Transaction loop : allTransactions) {

                balance = balance.add(loop.getValue(controller.getModel().getActiveWallet()));

                long loopTimeInMillis = loop.getUpdateTime().getTime();

                if (loopTimeInMillis > pastInMillis) {
                    if (!leftEdgeDataPointAdded) {
                        // If the previous transaction was BEFORE the
                        // NUMBER_OF_DAYS_TO_LOOK_BACK cutoff, include a
                        // datapoint
                        // at the beginning of the timewindow with the balance
                        // at
                        // that time.
                        if ((previousDate != null) && (previousDate.getTime() <= pastInMillis)) {
                            // The balance was non-zero.
                            chartData.add(new ChartData(new Date(pastInMillis), previousBalance));
                        } else {
                            // At beginning of time window balance was zero
                            chartData.add(new ChartData(new Date(pastInMillis), BigInteger.ZERO));
                        }
                        leftEdgeDataPointAdded = true;
                    }

                    // Include this transaction as it is in the last
                    // NUMBER_OF_DAYS_TO_LOOK_BACK days.
                    chartData.add(new ChartData(loop.getUpdateTime(), balance));
                }

                previousBalance = balance;
                previousDate = loop.getUpdateTime();
            }
        } catch (com.google.bitcoin.core.ScriptException e1) {
            e1.printStackTrace();
        }

        return chartData;
    }

    @Override
    /**
     * Release any resources used when user navigates away from this view.
     */
    public void navigateAwayFromView() {

    }

    @Override
    public void displayView() {

        updateChart();
    }

    @Override
    public Icon getViewIcon() {

        return ImageLoader.createImageIcon(ImageLoader.CHART_LINE_ICON_FILE);
    }

    @Override
    public String getViewTitle() {

        return controller.getLocaliser().getString("chartsPanelAction.text");
    }

    @Override
    public String getViewTooltip() {

        return controller.getLocaliser().getString("chartsPanelAction.tooltip");
    }

    @Override
    public int getViewId() {

        return View.CHARTS_VIEW;
    }

    class ChartData {

        private Date date;
        private BigInteger value;

        public ChartData(Date date, BigInteger value) {

            this.date = date;
            this.value = value;
        }

        public Date getDate() {

            return date;
        }

        public BigInteger getValue() {

            return value;
        }

        @Override
        public String toString() {

            return "ChartData [date=" + date + ", value=" + value + "]";
        }
    }

    @Override
    public void componentHidden(ComponentEvent arg0) {
    }

    @Override
    public void componentMoved(ComponentEvent arg0) {
    }

    @Override
    public void componentResized(ComponentEvent arg0) {
        updateChart();
    }

    @Override
    public void componentShown(ComponentEvent arg0) {
    }
}