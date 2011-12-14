package org.multibit.view;

import java.io.*;

/**
 * Reads file in UTF-8 encoding and output to STDOUT in ASCII with unicode
 * escaped sequence for characters outside of ASCII.
 */
public class UTF8ToAscii {
    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
        System.out.println("Usage: java UTF8ToAscii <filename>");
        return;
    }

    BufferedReader r = new BufferedReader(
                new InputStreamReader(
                    new FileInputStream(args[0]),
                    "UTF-8"
                )
               );
    String line = r.readLine();
    while (line != null) {
        System.out.println(unicodeEscape(line));
        line = r.readLine();
    }
    r.close();
    }

    private static final char[] hexChar = {
        '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
    };

    private static String unicodeEscape(String s) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < s.length(); i++) {
        char c = s.charAt(i);
        if ((c >> 7) > 0) {
        sb.append("\\u");
        sb.append(hexChar[(c >> 12) & 0xF]); // append the hex character for the left-most 4-bits
        sb.append(hexChar[(c >> 8) & 0xF]);  // hex for the second group of 4-bits from the left
        sb.append(hexChar[(c >> 4) & 0xF]);  // hex for the third group
        sb.append(hexChar[c & 0xF]);         // hex for the last group, e.g., the right most 4-bits
        }
        else {
        sb.append(c);
        }
    }
    return sb.toString();
    }
}
