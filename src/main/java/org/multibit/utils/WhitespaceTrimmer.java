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
package org.multibit.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WhitespaceTrimmer {

    private static String whiteRange = "\\p{javaWhitespace}\\p{Zs}";
    private static Pattern whiteStart = Pattern.compile("^[" + whiteRange + "]+");
    private static Pattern whiteEnd = Pattern.compile("[" + whiteRange + "]+$");

    private WhitespaceTrimmer() {
    }

    static String ltrim(String text) {
        if (text == null) {
            return "";
        }
        Matcher mStart = whiteStart.matcher(text);
        return mStart.find() ? text.substring(mStart.end()) : text;
    }

    static String rtrim(String text) {
        if (text == null) {
            return "";
        }
        Matcher mEnd = whiteEnd.matcher(text);
        if (mEnd.find()) {
            int matchStart = mEnd.start();
            return text.substring(0, matchStart);
        } else {
            return text;
        }
    }

    public static String trim(String text) {
        return (rtrim(ltrim(text.trim()))).trim();
    }
}
