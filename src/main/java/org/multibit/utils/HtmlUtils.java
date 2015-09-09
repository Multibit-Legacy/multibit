package org.multibit.utils;

/**
 *  <p>[Pattern] to provide the following to [related classes]:<br>
 *  <ul>
 *  <li></li>
 *  </ul>
 *  Example:<br>
 *  <pre>
 *  </pre>
 *  </p>
 *  
 */
public class HtmlUtils {
  /**
     * @param lines The lines to wrap in HTML
     *
     * @return A single block of HTML that provides appropriate text alignment (LTR or RTL) and line breaks for the locale
     */
    public static String localiseCenteredWithLineBreaks(String[] lines) {

      final StringBuilder sb = new StringBuilder("<html><body style='width: 100%'><div align=center>");

      // Wrap in paragraphs to ensure word wrap
      boolean first = true;
      for (String line : lines) {
        if (!first) {
          sb.append("<br>");
        }
        sb.append("<p>")
          .append(line)
          .append("</p>");
        first = false;
      }
      sb.append("</div></body></html>");

      return sb.toString();
    }
}
