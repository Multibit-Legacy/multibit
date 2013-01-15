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
package org.multibit.viewsystem.swing;

import org.multibit.viewsystem.ViewSystem;

/**
 *
 * @author Cameron Garnham
 */
public interface CommonFrame extends ViewSystem {
    String EXAMPLE_LONG_FIELD_TEXT = "1JiM1UyTGqpLqgayxTPbWbcdVeoepmY6pK++++";
    String EXAMPLE_MEDIUM_FIELD_TEXT = "Typical phrase 0.12345678 BTC ($0.01)";
    int HEIGHT_OF_HEADER = 70;
    int ON_TRANSACTION_CONFIDENCE_CHANGE_DELAY = 50;
    int SCROLL_BAR_DELTA = 20;
    String SEPARATOR = " - ";
    int WALLET_WIDTH_DELTA = 30;
    int WIDTH_OF_AMOUNT_FIELD = 150;
    int WIDTH_OF_LONG_FIELDS = 300;

    void updateHeader();
    
}
