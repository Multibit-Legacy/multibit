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
package org.multibit.viewsystem.swing.view.panels;


import com.google.bitcoin.core.Transaction;
import com.xeiam.xchart.*;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.model.core.CoreModel;
import org.multibit.utils.DateUtils;
import org.multibit.utils.ImageLoader;
import org.multibit.viewsystem.DisplayHint;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.Viewable;
import org.multibit.viewsystem.swing.ColorAndFontConstants;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * The Charts view.
 */
public class ChartsPanel extends JPanel implements Viewable, ComponentListener {

  private Logger log = LoggerFactory.getLogger(ChartsPanel.class);

  private static final long serialVersionUID = 191352212345998705L;

  private static final int NUMBER_OF_DAYS_TO_LOOK_BACK = 30;
  private static final double NUMBER_OF_SATOSHI_IN_ONE_BTC = 100000000;

  private static final int WIDTH_DELTA = 40;
  private static final int HEIGHT_DELTA = 20;

  private static final int MINIMUM_WIDTH = 800;
  private static final int MINIMUM_HEIGHT = 300;

  private static final String DATE_PATTERN = "dd-MMM";

  private final Controller controller;
  private final BitcoinController bitcoinController;

  private JPanel mainPanel;

  private boolean generateRandomChart = false;

  /**
   * Creates a new {@link ChartsPanel}.
   */
    public ChartsPanel(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
    this.bitcoinController = bitcoinController;
    this.controller = this.bitcoinController;

    setLayout(new BorderLayout());
    setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    setForeground(ColorAndFontConstants.TEXT_COLOR);

    setOpaque(true);
    applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    mainFrame.addComponentListener(this);
    addComponentListener(this);

    initUI();
  }

  private void initUI() {
    mainPanel = new JPanel();
    mainPanel.setLayout(new GridBagLayout());
    mainPanel.setOpaque(true);
    mainPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    mainPanel.setForeground(ColorAndFontConstants.TEXT_COLOR);

    mainPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    GridBagConstraints constraints = new GridBagConstraints();

    constraints.fill = GridBagConstraints.BOTH;
    constraints.gridx = 0;
    constraints.gridy = 0;
    constraints.gridwidth = 1;
    constraints.weightx = 1;
    constraints.weighty = 1;
    constraints.anchor = GridBagConstraints.CENTER;

    // Initially blank.
    JPanel chartPanel = new JPanel();
    chartPanel.setOpaque(true);
    chartPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    chartPanel.setForeground(ColorAndFontConstants.TEXT_COLOR);

    mainPanel.add(chartPanel, constraints);

    JScrollPane mainScrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    mainScrollPane.setBorder(BorderFactory.createEmptyBorder());
    mainScrollPane.getViewport().setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    mainScrollPane.getViewport().setForeground(ColorAndFontConstants.TEXT_COLOR);
    mainScrollPane.getViewport().setOpaque(true);
    mainScrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
    mainScrollPane.getHorizontalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);
    mainScrollPane.getVerticalScrollBar().setUnitIncrement(CoreModel.SCROLL_INCREMENT);

    add(mainScrollPane, BorderLayout.CENTER);
  }

  /**
   * Create a panel containing the chart to show.
   */
  private JPanel createChartPanel() {
    try {
      setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
      setForeground(ColorAndFontConstants.TEXT_COLOR);

      int chartWidth = Math.max(getWidth() - WIDTH_DELTA, MINIMUM_WIDTH);
      int chartHeight = Math.max(getHeight() - HEIGHT_DELTA, MINIMUM_HEIGHT);
      Chart chart = new Chart(chartWidth, chartHeight);
      Locale locale = controller.getLocaliser().getLocale();
      chart.getStyleManager().setLocale(locale);

      // generates linear data
      Collection<Date> xData = new ArrayList<Date>();
      Collection<Number> yData = new ArrayList<Number>();

      // Get the last month's transaction data.
      Collection<ChartData> chartDataCollection = getChartData();

      if (generateRandomChart) {
        DateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
        Date date;
        for (int i = 1; i <= 10; i++) {
          try {
            date = sdf.parse(i + ".10.2008");
            xData.add(date);
          } catch (ParseException e) {
            e.printStackTrace();
          }
          yData.add(Math.random() * i);
        }
      } else {
        if (chartDataCollection == null || chartDataCollection.size() == 0) {
          log.debug("chartDataCollection is null or empty");

          JPanel chartPanel = new JPanel();
          chartPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
          chartPanel.setForeground(ColorAndFontConstants.TEXT_COLOR);
          chartPanel.setOpaque(true);
          return chartPanel;
        } else {
          for (ChartData chartData : chartDataCollection) {
            if (chartData != null && chartData.getDate() != null && chartData.getValue() != null) {
              xData.add(chartData.getDate());
              yData.add(chartData.getValue().doubleValue() / NUMBER_OF_SATOSHI_IN_ONE_BTC);
            }
          }
        }
      }

      // Customize Chart.
      String xAxisLabel = controller.getLocaliser().getString("walletData.dateText");
      String currencyUnitSuffix = " (" + controller.getLocaliser().getString("sendBitcoinPanel.amountUnitLabel") + ")";
      String balanceLabel = controller.getLocaliser().getString("multiBitFrame.balanceLabel") + currencyUnitSuffix;
      String unitOfTime = controller.getLocaliser().getString("chartsPanelTitle.days");
      String chartTitle = controller.getLocaliser().getString("chartsPanelTitle.text", new Object[] { NUMBER_OF_DAYS_TO_LOOK_BACK, unitOfTime }) + currencyUnitSuffix;

      chart.getStyleManager().setPlotGridLinesVisible(false);
      chart.getStyleManager().setXAxisTicksVisible(true);
      chart.getStyleManager().setLegendVisible(false);

      chart.getStyleManager().setChartBackgroundColor(ColorAndFontConstants.BACKGROUND_COLOR);
      chart.getStyleManager().setChartFontColor(ColorAndFontConstants.TEXT_COLOR);
      chart.getStyleManager().setAxisTickLabelsColor(ColorAndFontConstants.TEXT_COLOR);
      chart.getStyleManager().setAxisTickMarksColor(ColorAndFontConstants.TEXT_COLOR);
      chart.getStyleManager().setChartTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(2));
      chart.getStyleManager().setAxisTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
      chart.getStyleManager().setAxisTickLabelsFont(FontSizer.INSTANCE.getAdjustedDefaultFontWithDelta(-2));
      chart.getStyleManager().setDatePattern(DATE_PATTERN);

      chart.setChartTitle(chartTitle);
      chart.setXAxisTitle(xAxisLabel);

      com.xeiam.xchart.Series series = chart.addDateSeries(balanceLabel, xData, yData);
      series.setLineColor(SeriesColor.BLUE);
      series.setMarkerColor(SeriesColor.BLUE);
      series.setMarker(SeriesMarker.CIRCLE);
      series.setLineStyle(SeriesLineStyle.SOLID);

      XChartPanel chartPanelToReturn = new XChartPanel(chart);
      chartPanelToReturn.setSaveAsString(controller.getLocaliser().getString("chartsPanelSaveAs"));
      chartPanelToReturn.setLocale(locale);
      chartPanelToReturn.setMinimumSize(new Dimension(chartWidth, chartHeight));
      return chartPanelToReturn;
    } catch (Exception e) {
      e.printStackTrace();
      JPanel chartPanel = new JPanel();
      chartPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
      chartPanel.setForeground(ColorAndFontConstants.TEXT_COLOR);
      chartPanel.setOpaque(true);
      return chartPanel;
    }
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

    // Recreate chart panel.
    JPanel chartPanel = createChartPanel();
    chartPanel.setOpaque(true);
    chartPanel.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
    chartPanel.setForeground(ColorAndFontConstants.TEXT_COLOR);
    mainPanel.add(chartPanel, constraints);
  }

  /**
   * Get the transaction data for the chart
   */
  private Collection<ChartData> getChartData() {

    if (controller.getModel() == null || this.bitcoinController.getModel().getActiveWallet() == null) {
      return new ArrayList<ChartData>();
    }

    ArrayList<Transaction> allTransactions = new ArrayList<Transaction>(this.bitcoinController.getModel().getActiveWallet().getTransactions(false));

    // Order by date.
    Collections.sort(allTransactions, new Comparator<Transaction>() {

      @Override
      public int compare(Transaction t1, Transaction t2) {

        Date date1 = t1.getUpdateTime();
        Date date2 = t2.getUpdateTime();
        if (date1 == null) {
          if (date2 == null) {
            return 0;
          } else {
            return -1;
          }
        } else {
          if (date2 == null) {
            return 1;
          } else {
            return date1.compareTo(date2);
          }
        }

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

      if (allTransactions == null || allTransactions.size() == 0) {
        // At beginning of time window balance was zero
        chartData.add(new ChartData(new Date(pastInMillis), BigInteger.ZERO));
      } else {
        for (Transaction loop : allTransactions) {
          balance = balance.add(loop.getValue(this.bitcoinController.getModel().getActiveWallet()));

          Date loopUpdateTime = loop.getUpdateTime();
          if (loopUpdateTime != null) {
            long loopTimeInMillis = loopUpdateTime.getTime();

            if (loopTimeInMillis > pastInMillis) {
              if (!leftEdgeDataPointAdded) {
                // If the previous transaction was BEFORE the
                // NUMBER_OF_DAYS_TO_LOOK_BACK cutoff, include a
                // datapoint at the beginning of the timewindow
                // with the balance
                // at that time.
                if ((previousDate != null) && (previousDate.getTime() <= pastInMillis)) {
                  // The balance was non-zero.
                  chartData.add(new ChartData(new Date(pastInMillis), previousBalance));
                } else {
                  // At beginning of time window balance was
                  // zero
                  chartData.add(new ChartData(new Date(pastInMillis), BigInteger.ZERO));
                }
                leftEdgeDataPointAdded = true;
              }

              // Include this transaction as it is in the last
              // NUMBER_OF_DAYS_TO_LOOK_BACK days.
              chartData.add(new ChartData(loop.getUpdateTime(), previousBalance));
              chartData.add(new ChartData(loop.getUpdateTime(), balance));
            }

            previousBalance = balance;
            previousDate = loop.getUpdateTime();
          }
        }
      }

      // If all the datapoints are before the left hand edge, ensure the balance is also added at the left hand edge.
      if (!leftEdgeDataPointAdded) {
        chartData.add(new ChartData(new Date(pastInMillis), balance));
      }

      // Add in the balance at the end of the time window.
      chartData.add(new ChartData(new Date(DateUtils.nowUtc().getMillis()), balance));
      // log.debug("Last transaction date = " + previousDate + ", chart balance = " + balance + ", wallet balance = " + controller.getModel().getActiveWallet().getBalance());
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
  public void displayView(DisplayHint displayHint) {

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
  public View getViewId() {

    return View.CHARTS_VIEW;
  }

  static class ChartData {

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