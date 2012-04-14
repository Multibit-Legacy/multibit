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

import java.util.ArrayList;

import javax.swing.table.AbstractTableModel;

import org.multibit.controller.MultiBitController;
import org.multibit.model.ExchangeData;
import org.multibit.model.MultiBitModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Table model for ticker
 * 
 * @author jim
 * 
 */
public class TickerTableModel extends AbstractTableModel {

    public static final String TICKER_COLUMN_CURRENCY = "currency";
    public static final String TICKER_COLUMN_RATE = "rate";
    public static final String TICKER_COLUMN_BID = "bid";
    public static final String TICKER_COLUMN_ASK = "ask";
    public static final String TICKER_COLUMN_EXCHANGE = "exchange";

    private static final String[] COLUMN_HEADER_BID_ASK_KEYS = new String[] { "tickerTableModel.symbol", "tickerTableModel.bid",
            "tickerTableModel.ask", "tickerTableModel.exchange" };

    private static final String[] COLUMN_HEADER_RATE_KEYS = new String[] { "tickerTableModel.symbol", "tickerTableModel.rate",
            "tickerTableModel.exchange" };

    private static final long serialVersionUID = -775886012854496208L;

    private static final Logger log = LoggerFactory.getLogger(TickerTableModel.class);

    private ArrayList<String> headers;

    /**
     * The exchange data
     */
    private ExchangeData exchangeData;

    private boolean showSecondRow;

    private String exchange1;
    private String currency1;

    private String exchange2;
    private String currency2;

    /**
     * the MultiBit model
     */
    private MultiBitModel multiBitModel;

    private MultiBitController controller;

    private boolean showCurrency;
    private boolean showRate;
    private boolean showBid;
    private boolean showAsk;
    private boolean showExchange;

    private String[] columnVariables;
    private int numberOfColumns;

    public TickerTableModel(MultiBitController controller) {
        this.multiBitModel = controller.getModel();
        this.controller = controller;

        String tickerColumnsToShow = "" + controller.getModel().getUserPreference(MultiBitModel.TICKER_COLUMNS_TO_SHOW);

        showCurrency = tickerColumnsToShow.indexOf(TICKER_COLUMN_CURRENCY) > -1;
        showRate = tickerColumnsToShow.indexOf(TICKER_COLUMN_RATE) > -1;
        showBid = tickerColumnsToShow.indexOf(TICKER_COLUMN_BID) > -1;
        showAsk = tickerColumnsToShow.indexOf(TICKER_COLUMN_ASK) > -1;
        showExchange = tickerColumnsToShow.indexOf(TICKER_COLUMN_EXCHANGE) > -1;

        columnVariables = new String[5]; // 5 = max number of columns
        
        numberOfColumns = 0;
        if (showCurrency) {
            columnVariables[numberOfColumns] = TICKER_COLUMN_CURRENCY;
            numberOfColumns++;
        }
        if (showRate) {
            columnVariables[numberOfColumns] = TICKER_COLUMN_RATE;
            numberOfColumns++;
        }
        if (showBid) {
            columnVariables[numberOfColumns] = TICKER_COLUMN_BID;
            numberOfColumns++;
        }
        if (showAsk) {
            columnVariables[numberOfColumns] = TICKER_COLUMN_ASK;
            numberOfColumns++;
        }
        if (showExchange) {
            columnVariables[numberOfColumns] = TICKER_COLUMN_EXCHANGE;
            numberOfColumns++;
        }

        showSecondRow = Boolean.TRUE.toString().equals(
                controller.getModel().getUserPreference(MultiBitModel.TICKER_SHOW_SECOND_ROW));

        exchange1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_EXCHANGE);
        if (exchange1 == null || "".equals(exchange1)) {
            exchange1 = ExchangeData.DEFAULT_EXCHANGE;
        }

        currency1 = controller.getModel().getUserPreference(MultiBitModel.TICKER_FIRST_ROW_CURRENCY);
        if (currency1 == null || "".equals(currency1)) {
            currency1 = ExchangeData.DEFAULT_CURRENCY;
        }

        exchange2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_EXCHANGE);
        if (exchange2 == null || "".equals(exchange2)) {
            exchange2 = ExchangeData.DEFAULT_EXCHANGE;
        }
        currency2 = controller.getModel().getUserPreference(MultiBitModel.TICKER_SECOND_ROW_CURRENCY);
        if (currency2 == null || "".equals(currency2)) {
            currency2 = ExchangeData.DEFAULT_CURRENCY;
        }

        createHeaders();

        exchangeData = multiBitModel.getExchangeData();
    }

    public int getColumnCount() {
        return numberOfColumns;
    }

    public int getRowCount() {
        if (showSecondRow) {
            return 2;
        } else {
            return 1;
        }
    }

    public String getColumnName(int column) {
        return controller.getLocaliser().getString("tickerTableModel." + columnVariables[column]);
    }

    public Object getValueAt(int row, int column) {
        if (row < 0 && row >= getRowCount()) {
            return null;
        }

        String exchange;
        String currency;
        if (row == 0) {
            exchange = exchange1;
            currency = currency1;
        } else {
            exchange = exchange2;
            currency = currency2;
        }

        String variable = columnVariables[column];
        
        if (TICKER_COLUMN_CURRENCY.equals(variable)) {
            // currency
            return currency;
        } else if (TICKER_COLUMN_RATE.equals(variable)) {
            // rate
            if (exchangeData.getLastRate(currency) < 0) {
                return " ";
            } else {
                return String.format("%1$,.5f", exchangeData.getLastRate(currency));
            }
        } else if (TICKER_COLUMN_BID.equals(variable)) {
            // bid
            if (exchangeData.getLastBid(currency) < 0) {
                return " ";
            } else {
                return String.format("%1$,.5f", exchangeData.getLastBid(currency));
            }
        } else if (TICKER_COLUMN_ASK.equals(variable)) {
            // ask
            if (exchangeData.getLastAsk(currency) < 0) {
                return " ";
            } else {
                return String.format("%1$,.5f", exchangeData.getLastAsk(currency));
            }
        } else if (TICKER_COLUMN_EXCHANGE.equals(variable)) {
            // exchange
            return exchange;
        } else {
            // do not know 
            return "";
        }
    }

    /**
     * table model is read only
     */
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public void createHeaders() {
        headers = new ArrayList<String>();
        if (showRate) {
            for (int j = 0; j < COLUMN_HEADER_RATE_KEYS.length; j++) {
                headers.add(controller.getLocaliser().getString(COLUMN_HEADER_RATE_KEYS[j]));
            }
        } else {
            for (int j = 0; j < COLUMN_HEADER_BID_ASK_KEYS.length; j++) {
                headers.add(controller.getLocaliser().getString(COLUMN_HEADER_BID_ASK_KEYS[j]));
            }
        }
    }

    public boolean isShowRate() {
        return showRate;
    }
}
