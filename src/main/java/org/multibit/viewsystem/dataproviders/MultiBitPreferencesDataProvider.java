/*
 * The MIT License
 *
 * Copyright 2013 Cameron Garnham.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.multibit.viewsystem.dataproviders;

/**
 *
 * @author Cameron Garnham
 */
public interface MultiBitPreferencesDataProvider  {

    String getNewCurrency1();

    String getNewCurrency2();

    String getNewExchange1();

    String getNewExchange2();

    /**
     * Get the new send fee
     */
    String getNewSendFee();

    boolean getNewShowAsk();

    boolean getNewShowBid();

    boolean getNewShowBitcoinConvertedToFiat();

    boolean getNewShowCurrency();

    boolean getNewShowExchange();

    boolean getNewShowRate();

    boolean getNewShowSecondRow();

    boolean getNewShowTicker();

    String getPreviousCurrency1();

    String getPreviousCurrency2();

    String getPreviousExchange1();

    String getPreviousExchange2();

    /**
     * Get the previous send fee
     */
    String getPreviousSendFee();

    boolean getPreviousShowAsk();

    boolean getPreviousShowBid();

    boolean getPreviousShowBitcoinConvertedToFiat();

    /**
     * ticker information
     * @return
     */
    boolean getPreviousShowCurrency();

    boolean getPreviousShowExchange();

    boolean getPreviousShowRate();

    boolean getPreviousShowSecondRow();

    boolean getPreviousShowTicker();

    boolean isTickerVisible();
    
}
