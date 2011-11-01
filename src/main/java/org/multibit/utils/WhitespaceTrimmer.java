package org.multibit.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WhitespaceTrimmer {

    private static String whiteRange = "\\p{javaWhitespace}\\p{Zs}";
    private static Pattern whiteStart = Pattern.compile("^[" + whiteRange + "]+");
    private static Pattern whiteEnd = Pattern.compile("[" + whiteRange + "]+&");

    private WhitespaceTrimmer() {
    }

    public static String ltrim(String text) {
        if (text == null) {
            return "";
        }
        Matcher mStart = whiteStart.matcher(text);
        return mStart.find() ? text.substring(mStart.end()) : text;
    }

    public static String rtrim(String text) {
        if (text == null) {
            return "";
        }
        Matcher mEnd = whiteEnd.matcher(text);
        return mEnd.find() ? text.substring(0, mEnd.start()) : text;
    }

    public static String trim(String text) {
        return (rtrim(ltrim(text))).trim();
    }
}
