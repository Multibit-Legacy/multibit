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
package org.multibit.model;

/**
 * 
 * @author timmolter
 * 
 */
public class ExchangeData {

    public static final String MT_GOX_EXCHANGE_NAME = "MtGox";

    private double lastTickUSD = -1; // -1 = do not know

    public double getLastTickUSD() {
        return lastTickUSD;
    }

    public void setLastTickUSD(double lastTickUSD) {
        this.lastTickUSD = lastTickUSD;
    }

    public String[] getAvailableExchanges() {
        return new String[] { MT_GOX_EXCHANGE_NAME };
    }

    public String[] getAvailableCurrenciesForExchange(String exchangeName) {
        return new String[] { "USD" };
    }
}
