package org.multibit.viewsystem.swing.view.components;

import org.multibit.Localiser;
import org.multibit.controller.Controller;
import org.multibit.utils.HtmlUtils;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Hashtable;

/**
 * <p>Factory to provide the following to UI:</p>
 * <ul>
 * <li>Produce a slider</li>
 * </ul>
 *
 * @since 0.0.1
 */
public class FeeSlider {

  public static final long MINIMUM_FEE_PER_KB = 1000;   // Slightly higher than the minimum relay fee (1000 sat per KB)  as per Bitcoin Core 0.9
  public static final long DEFAULT_FEE_PER_KB = 10000;  // 0.1 mBTC per KB - a long used fee structure which works as of spam attacks of July 2015
  public static final long MAXIMUM_FEE_PER_KB = 50000;  // 0.5 mBTC per KB

  public static final BigDecimal NUMBER_OF_SATOSHI_IN_A_BITCOIN = new BigDecimal(100000000);
  public static final int SCALE = 8;

  /**
   * Resolution of a single tick of the slider, in satoshi
   */
  public static final int RESOLUTION = 200;

  /**
   * Utilities have no public constructor
   */
  private FeeSlider() {
  }

  /**
   * <p>Create a new slider to adjust transaction fee</p>
   *
   * @param changeListener  The change listener to respond to adjustments
   * @param initialPosition The initial position to choose on the slider, in satoshi
   */
  public static JSlider newFeeSlider(Controller controller, ChangeListener changeListener, long initialPosition) {
    // Resolution is RESOLUTION satoshis per tick
    int minimumPosition = (int) MINIMUM_FEE_PER_KB / RESOLUTION;
    int defaultPosition = (int) DEFAULT_FEE_PER_KB / RESOLUTION;
    int maximumPosition = (int) MAXIMUM_FEE_PER_KB / RESOLUTION;

    // Make sure feePerKB is normalised first so that it will be in range of the slider
    int currentPosition = (int) normaliseFeePerKB(initialPosition) / RESOLUTION;
    JSlider feePerKBSlider = new JSlider(minimumPosition, maximumPosition,
            currentPosition);

    feePerKBSlider.setMajorTickSpacing(10);
    feePerKBSlider.setPaintTicks(true);
    feePerKBSlider.setPreferredSize(new Dimension(400, 120));
    feePerKBSlider.setOpaque(false);
    feePerKBSlider.setBorder(BorderFactory.createEmptyBorder());
    feePerKBSlider.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

    Localiser localiser = controller.getLocaliser();

    // Create the label table
    Hashtable<Integer, JComponent> labelTable = new Hashtable<>();

    JLabel lowerLabel = new JLabel(localiser.getString("sliders.lower"));
    lowerLabel.setOpaque(false);
    lowerLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

    JLabel defaultLabel = newDefaultNote(localiser);
    defaultLabel.setOpaque(false);
    defaultLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

    JLabel upperLabel =  new JLabel(localiser.getString("sliders.higher"));
    upperLabel.setOpaque(false);
    upperLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());

    labelTable.put(minimumPosition, lowerLabel);
    labelTable.put(defaultPosition, defaultLabel);
    labelTable.put(maximumPosition, upperLabel);
    feePerKBSlider.setLabelTable(labelTable);
    feePerKBSlider.setPaintLabels(true);

    feePerKBSlider.addChangeListener(changeListener);

    return feePerKBSlider;
  }

  /**
   * Parse and normalise the feePerKB so that it is always between the minimum and maximum values
   *
   * @param rawFeePerKB the raw value of feePerKB, as String. From user preferences so may not be a number
   * @return the normalised feePerKB, as a long/ satoshi
   */
  public static long parseAndNormaliseFeePerKB(String rawFeePerKB) {
    long feeValue;
    if (rawFeePerKB == null) {
      feeValue = FeeSlider.DEFAULT_FEE_PER_KB;
    } else {
      try {
        feeValue = Long.parseLong(rawFeePerKB);
      } catch (NumberFormatException nfe) {
        // Value in multibit.properties was not a number - use the default value
        feeValue = FeeSlider.DEFAULT_FEE_PER_KB;
      }
    }
    return normaliseFeePerKB(feeValue);
  }

  /**
   * Normalise the feePerKB so that it is always between the minimum and maximum values
   *
   * @param rawFeePerKB the raw value of feePerKB, as long/ satoshi
   * @return the normalised feePerKB, as a long/ satoshi
   */
  public static long normaliseFeePerKB(long rawFeePerKB) {
    if (rawFeePerKB == 0) {
      return DEFAULT_FEE_PER_KB;
    }

    if (rawFeePerKB < MINIMUM_FEE_PER_KB) {
      return MINIMUM_FEE_PER_KB;
    }

    if (rawFeePerKB > MAXIMUM_FEE_PER_KB) {
      return MAXIMUM_FEE_PER_KB;
    }

    // Ok as is
    return rawFeePerKB;
  }

  /**
   * @return A new "default" note for use on the Fee slider
   */
  private static JLabel newDefaultNote(Localiser localiser) {
    // Wrap in HTML to ensure LTR/RTL and line breaks are respected
    String[] lines = new String[2];
    lines[0] = "\u007c"; // ASCII vertical line // 25B2 =up black triangle
    lines[1] = localiser.getString("sliders.default");
    JLabel label = new JLabel(HtmlUtils.localiseCenteredWithLineBreaks(lines));
    label.setHorizontalAlignment(SwingConstants.CENTER);
    return label;
  }

  public static String convertSatoshiToString(long satoshi) {
    BigDecimal satoshiBigDecimal = new BigDecimal(satoshi);
    return satoshiBigDecimal.divide(FeeSlider.NUMBER_OF_SATOSHI_IN_A_BITCOIN, FeeSlider.SCALE, RoundingMode.HALF_EVEN).toPlainString();
  }
}