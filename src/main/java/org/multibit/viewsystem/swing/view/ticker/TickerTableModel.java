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

import org.multibit.controller.Controller;
import org.multibit.controller.exchange.ExchangeController;
import org.multibit.model.exchange.ExchangeData;
import org.multibit.model.exchange.ExchangeModel;

import javax.swing.table.AbstractTableModel;

/**
 * Table model for ticker.
 * 
 * @author jim
 * 
 */
public class TickerTableModel extends AbstractTableModel {

    public static final String TICKER_COLUMN_NONE = "none";
    public static final String TICKER_COLUMN_CURRENCY = "currency";
    public static final String TICKER_COLUMN_LAST_PRICE = "lastPrice";
    public static final String TICKER_COLUMN_BID = "bid";
    public static final String TICKER_COLUMN_ASK = "ask";
    public static final String TICKER_COLUMN_EXCHANGE = "exchange";

    private static final int MAX_NUMBER_OF_COLUMNS = 5;

    public static final String DEFAULT_COLUMNS_TO_SHOW = "currency lastPrice exchange";
    public static final String DEFAULT_CURRENCY = "USD";

    private static final long serialVersionUID = -775886012854496208L;

    private boolean showSecondRow;

    private String exchange1;
    private String currency1;

    private String exchange2;
    private String currency2;

    private final Controller controller;
    private final ExchangeController exchangeController;

    private boolean showCurrency;
    private boolean showLastPrice;
    private boolean showBid;
    private boolean showAsk;
    private boolean showExchange;

    private String[] columnVariables = new String[MAX_NUMBER_OF_COLUMNS];
    private String[] tempColumns = new String[MAX_NUMBER_OF_COLUMNS];
    private int numberOfColumns;

    public TickerTableModel(ExchangeController exchangeController) {
        this.exchangeController = exchangeController;
        this.controller = this.exchangeController;

        String tickerColumnsToShow = controller.getModel().getUserPreference(ExchangeModel.TICKER_COLUMNS_TO_SHOW);

        if (tickerColumnsToShow == null || tickerColumnsToShow.equals("")) {
            tickerColumnsToShow = DEFAULT_COLUMNS_TO_SHOW;
        }
        showCurrency = tickerColumnsToShow.indexOf(TICKER_COLUMN_CURRENCY) > -1;
        showLastPrice = tickerColumnsToShow.indexOf(TICKER_COLUMN_LAST_PRICE) > -1;
        showBid = tickerColumnsToShow.indexOf(TICKER_COLUMN_BID) > -1;
        showAsk = tickerColumnsToShow.indexOf(TICKER_COLUMN_ASK) > -1;
        showExchange = tickerColumnsToShow.indexOf(TICKER_COLUMN_EXCHANGE) > -1;

        numberOfColumns = 0;
        if (showExchange) {
        	tempColumns[numberOfColumns] = TICKER_COLUMN_EXCHANGE;
        	numberOfColumns++;
        }
        if (showCurrency) {
            tempColumns[numberOfColumns] = TICKER_COLUMN_CURRENCY;
            numberOfColumns++;
        }
        if (showLastPrice) {
            tempColumns[numberOfColumns] = TICKER_COLUMN_LAST_PRICE;
            numberOfColumns++;
        }
        if (showBid) {
            tempColumns[numberOfColumns] = TICKER_COLUMN_BID;
            numberOfColumns++;
        }
        if (showAsk) {
            tempColumns[numberOfColumns] = TICKER_COLUMN_ASK;
            numberOfColumns++;
        }

        columnVariables = new String[numberOfColumns];
        System.arraycopy(tempColumns, 0, columnVariables, 0, numberOfColumns);

        showSecondRow = Boolean.TRUE.toString().equals(
                controller.getModel().getUserPreference(ExchangeModel.TICKER_SHOW_SECOND_ROW));

        exchange1 = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE);
        if (exchange1 == null || "".equals(exchange1) || "null".equals(exchange1)) {
            exchange1 = ExchangeData.DEFAULT_EXCHANGE;
        }

        currency1 = controller.getModel().getUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY);
        if (currency1 == null || "".equals(currency1) || "null".equals(currency1)) {
            currency1 = ExchangeData.DEFAULT_CURRENCY;
        }

        // Map MtGox to Bitstamp
        if (ExchangeData.MT_GOX_EXCHANGE_NAME.equalsIgnoreCase(exchange1)) {
          exchange1 = ExchangeData.BITSTAMP_EXCHANGE_NAME;
          controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_EXCHANGE, ExchangeData.BITSTAMP_EXCHANGE_NAME);

          currency1 = "USD";
          controller.getModel().setUserPreference(ExchangeModel.TICKER_FIRST_ROW_CURRENCY, "USD");
        }

        exchange2 = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE);
        if (exchange2 == null || "".equals(exchange2) || "null".equals(exchange2)) {
            exchange2 = ExchangeData.DEFAULT_EXCHANGE;
        }

        currency2 = controller.getModel().getUserPreference(ExchangeModel.TICKER_SECOND_ROW_CURRENCY);
        if (currency2 == null || "".equals(currency2) || "null".equals(currency2)) {
            currency2 = ExchangeData.DEFAULT_CURRENCY;
        }

              // Map MtGox to Bitstamp
        if (ExchangeData.MT_GOX_EXCHANGE_NAME.equalsIgnoreCase(exchange2)) {
          exchange2 = ExchangeData.BITSTAMP_EXCHANGE_NAME;
          controller.getModel().setUserPreference(ExchangeModel.TICKER_SECOND_ROW_EXCHANGE, ExchangeData.BITSTAMP_EXCHANGE_NAME);

          currency2 = "USD";
          controller.getModel().setUserPreference(ExchangeModel.TICKER_SECOND_ROW_CURRENCY, "USD");
        }
    }

    @Override
    public int getColumnCount() {
        return numberOfColumns;
    }

    @Override
    public int getRowCount() {
        if (showSecondRow) {
            return 2;
        } else {
            return 1;
        }
    }

    @Override
    public String getColumnName(int column) {
        return controller.getLocaliser().getString("tickerTableModel." + columnVariables[column]);
    }

    @Override
    public Object getValueAt(int row, int column) {
        if (row < 0 && row >= getRowCount()) {
            return null;
        }

        String exchange;
        String currency;
        ExchangeData exchangeData;
        if (row == 0) {
            exchange = exchange1;
            currency = currency1;
            exchangeData = this.exchangeController.getModel().getExchangeData(exchange1);
        } else {
            exchange = exchange2;
            currency = currency2;
            exchangeData = this.exchangeController.getModel().getExchangeData(exchange2);;
        }

        String variable = columnVariables[column];

        if (TICKER_COLUMN_CURRENCY.equals(variable)) {
            // currency
            return currency;
        } else if (TICKER_COLUMN_LAST_PRICE.equals(variable)) {
            // rate
            if (exchangeData == null || exchangeData.getLastPrice(currency) == null) {
                return " ";
            } else {
                return controller.getLocaliser().bigMoneyValueToString(exchangeData.getLastPrice(currency));
            }
        } else if (TICKER_COLUMN_BID.equals(variable)) {
            // bid
            if (exchangeData == null || exchangeData.getLastBid(currency) == null) {
                return " ";
            } else {
                return controller.getLocaliser().bigMoneyValueToString(exchangeData.getLastBid(currency));
            }
        } else if (TICKER_COLUMN_ASK.equals(variable)) {
            // ask
            if (exchangeData == null || exchangeData.getLastAsk(currency) == null) {
                return " ";
            } else {
                 return controller.getLocaliser().bigMoneyValueToString(exchangeData.getLastAsk(currency));
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
     * Table model is read only.
     */
    @Override
    public void setValueAt(Object value, int row, int column) {
        throw new UnsupportedOperationException();
    }

    public String[] getColumnVariables() {
        return columnVariables;
    }
}
