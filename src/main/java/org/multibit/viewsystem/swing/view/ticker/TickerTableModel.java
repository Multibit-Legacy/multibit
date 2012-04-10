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
 * @author jim
 *
 */
public class TickerTableModel extends AbstractTableModel {

    private static final String[] COLUMN_HEADER_KEYS = new String[] { "tickerTableModel.symbol",
        "tickerTableModel.bid", "tickerTableModel.ask",
        "tickerTableModel.exchange" };

    private static final long serialVersionUID = -775886012854496208L;

    private static final Logger log = LoggerFactory.getLogger(TickerTableModel.class);

    private ArrayList<String> headers;

    /**
     * The exchange data
     */
    private ExchangeData exchangeData;
    
    private static final int NUMBER_OF_CURRENCIES_IN_EXCHANGE_DATA = 2;

    /**
     * the MultiBit model
     */
    private MultiBitModel multiBitModel;

    private MultiBitController controller;

    public TickerTableModel(MultiBitController controller) {
        this.multiBitModel = controller.getModel();
        this.controller = controller;

        createHeaders();

        exchangeData = multiBitModel.getExchangeData();
    }

    public int getColumnCount() {
        return COLUMN_HEADER_KEYS.length;
    }

    public int getRowCount() {
        return NUMBER_OF_CURRENCIES_IN_EXCHANGE_DATA;
    }

    public String getColumnName(int column) {
        return headers.get(column);
    }

    public Object getValueAt(int row, int column) {
        if (row < 0 && row >= NUMBER_OF_CURRENCIES_IN_EXCHANGE_DATA) {
            return null;
        }

        switch (column) {
        case 0: {
            return "USD";
        }
        case 1: {
            // bid
            return String.format("%1$,.4f", exchangeData.getLastTickUSD());
        }
        case 2:
            //ask
            return String.format("%1$,.4f", exchangeData.getLastTickUSD());
        case 3:
            return "MtGox";
        default:
            return null;
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
        for (int j = 0; j < COLUMN_HEADER_KEYS.length; j++) {
            headers.add(controller.getLocaliser().getString(COLUMN_HEADER_KEYS[j]));
        }
    }
}
