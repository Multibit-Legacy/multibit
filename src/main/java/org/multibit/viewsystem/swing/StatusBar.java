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
package org.multibit.viewsystem.swing;

/**
 * L2FProd.com Common Components 7.3 License.
 *
 * Copyright 2005-2007 L2FProd.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.google.dogecoin.core.Block;
import org.multibit.controller.Controller;
import org.multibit.controller.bitcoin.BitcoinController;
import org.multibit.message.Message;
import org.multibit.message.MessageListener;
import org.multibit.model.core.StatusEnum;
import org.multibit.viewsystem.swing.action.MultiBitAction;
import org.multibit.viewsystem.swing.view.components.BlinkLabel;
import org.multibit.viewsystem.swing.view.components.FontSizer;
import org.multibit.viewsystem.swing.view.components.MultiBitButton;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.multibit.viewsystem.swing.view.panels.SendBitcoinPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Timer;

/**
 * StatusBar. <BR>
 * A status bar is made of multiple zones. A zone can be any JComponent.
 * <p/>
 * In MultiBit the StatusBar is responsible for :
 * <p/>
 * 1) Online/ Connecting/ Error status label
 * 2) Status messages - these are cleared after a period of time
 * 3) Synchronisation progress bar
 */
public class StatusBar extends JPanel implements MessageListener {

  private static final Logger log = LoggerFactory.getLogger(StatusBar.class);

  private static final long serialVersionUID = 7824115980324911080L;

  private static final int A_SMALL_NUMBER_OF_PIXELS = 100;
  private static final int A_LARGE_NUMBER_OF_PIXELS = 1000000;
  private static final int STATUSBAR_HEIGHT = 30;

  private static final double TOLERANCE = 0.0000001;

  public static final int ONLINE_LABEL_WIDTH_DELTA = 12;
  public static final int ONLINE_LABEL_HEIGHT_DELTA = 8;

  private JLabel onlineLabel;
  final private MultiBitButton statusLabel;
  private StatusEnum statusEnum;

  public static final long TIMER_REPEAT_TIME = 5000; // millisecond
  public static final int NUMBER_OF_REPEATS = 12;

  private Timer statusClearTimer;
  static boolean clearAutomatically = true;

  private HashMap<String, Component> idToZones;
  private Border zoneBorder;

  private final Controller controller;
  private final BitcoinController bitcoinController;

  private MultiBitFrame mainFrame;

  private JProgressBar syncProgressBar;

  private SimpleDateFormat dateFormatter;

  /**
   * Construct a new StatusBar.
   */
  public StatusBar(BitcoinController bitcoinController, MultiBitFrame mainFrame) {
    this.bitcoinController = bitcoinController;
    this.controller = this.bitcoinController;

    this.mainFrame = mainFrame;

    setLayout(LookAndFeelTweaks.createHorizontalPercentLayout(controller.getLocaliser().getLocale()));
    idToZones = new HashMap<String, Component>();
    setZoneBorder(BorderFactory.createEmptyBorder());
    setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
    setOpaque(true);
    setBorder(BorderFactory.createMatteBorder(2, 0, 2, 0, ColorAndFontConstants.MID_BACKGROUND_COLOR));

    applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    final BitcoinController finalController = this.bitcoinController;

    dateFormatter = new SimpleDateFormat("dd MMM yyyy HH:mm", controller.getLocaliser().getLocale());

    onlineLabel = new JLabel("");
    onlineLabel.setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
    onlineLabel.setBackground(ColorAndFontConstants.VERY_LIGHT_BACKGROUND_COLOR);
    onlineLabel.setOpaque(true);

    onlineLabel.setHorizontalAlignment(SwingConstants.CENTER);
    onlineLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    onlineLabel.addMouseListener(new MouseAdapter() {
      @Override
      public void mouseEntered(MouseEvent arg0) {
        int blockHeight = -1;
        if (finalController.getMultiBitService() != null) {
          if (finalController.getMultiBitService().getChain() != null) {
            if (finalController.getMultiBitService().getChain().getChainHead() != null) {
              blockHeight = finalController.getMultiBitService().getChain().getChainHead().getHeight();
              Block blockHeader = finalController.getMultiBitService().getChain().getChainHead().getHeader();
              Date blockTime = blockHeader.getTime();
              int numberOfPeers = 0;
              if (finalController.getMultiBitService().getPeerGroup() != null && finalController.getMultiBitService().getPeerGroup().getConnectedPeers() != null) {
                numberOfPeers = finalController.getMultiBitService().getPeerGroup().getConnectedPeers().size();
              }
              onlineLabel.setToolTipText(HelpContentsPanel.createMultilineTooltipText(new String[]{
                      finalController.getLocaliser().getString("multiBitFrame.numberOfBlocks",
                              new Object[]{blockHeight}),
                      finalController.getLocaliser().getString("multiBitFrame.blockDate",
                              new Object[]{dateFormatter.format(blockTime)}),
                      finalController.getLocaliser().getString("multiBitFrame.connectedTo",
                              new Object[]{numberOfPeers})}));
            }
          }
        }
      }
    });

    statusLabel = new MultiBitButton("");
    statusLabel.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
    statusLabel.setOpaque(true);
    statusLabel.setBorderPainted(false);
    statusLabel.setForeground(Color.BLACK);
    //statusLabel.setBorder(BorderFactory.createLineBorder(Color.CYAN));
    statusLabel.setFocusPainted(false);

    statusLabel.setContentAreaFilled(false);
    statusLabel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    // show messages action
    MultiBitAction showMessagesAction = new MultiBitAction(controller, null, null,
            "messagesPanel.title", "messagesPanel.mnemonic", org.multibit.viewsystem.View.MESSAGES_VIEW);
    statusLabel.setAction(showMessagesAction);
    statusLabel.setHorizontalAlignment(JButton.LEADING);
    String tooltipText = HelpContentsPanel.createMultilineTooltipText(new String[]{
            controller.getLocaliser().getString("multiBitFrame.statusBar.tooltip1"), "\n",
            controller.getLocaliser().getString("multiBitFrame.statusBar.tooltip2")});
    statusLabel.setToolTipText(tooltipText);

    int onlineWidth = Math.max(Math.max(
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                    controller.getLocaliser().getString("multiBitFrame.onlineText")),
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                    controller.getLocaliser().getString("multiBitFrame.offlineText"))),
            getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).stringWidth(
                    controller.getLocaliser().getString("multiBitFrame.errorText"))
    )
            + ONLINE_LABEL_WIDTH_DELTA;

    int onlineHeight = getFontMetrics(FontSizer.INSTANCE.getAdjustedDefaultFont()).getHeight() + ONLINE_LABEL_HEIGHT_DELTA;

    onlineLabel.setPreferredSize(new Dimension(onlineWidth, onlineHeight));
    int statusBarHeight = Math.max(STATUSBAR_HEIGHT, onlineHeight);
    setMaximumSize(new Dimension(A_LARGE_NUMBER_OF_PIXELS, statusBarHeight));
    setMaximumSize(new Dimension(A_SMALL_NUMBER_OF_PIXELS, statusBarHeight));

    syncProgressBar = new JProgressBar(0, 100);
    syncProgressBar.setValue(0);
    syncProgressBar.setStringPainted(false);
    syncProgressBar.setVisible(false);
    syncProgressBar.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);
    syncProgressBar.setOpaque(true);
    syncProgressBar.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

    JPanel filler = new JPanel();
    filler.setOpaque(true);
    filler.setBackground(ColorAndFontConstants.MID_BACKGROUND_COLOR);

    addZone("online", onlineLabel, "" + onlineWidth, "left");
    addZone("progressBar", syncProgressBar, "" + 200, "left");
    addZone("network", statusLabel, "*", "");
    addZone("filler2", filler, "0", "right");

    statusClearTimer = new java.util.Timer();
    statusClearTimer.schedule(new StatusClearTask(statusLabel), TIMER_REPEAT_TIME, TIMER_REPEAT_TIME);
  }

  /**
   * initialise the statusbar;
   */
  public void initialise() {
    updateOnlineStatusText(StatusEnum.CONNECTING);
    updateStatusLabel("", true);
  }

  /**
   * refresh online status text with existing value
   */
  public void refreshOnlineStatusText() {
    updateOnlineStatusText(statusEnum);
  }

  /**
   * Update online status text with new value.
   *
   * @param finalStatusEnum
   */
  public void updateOnlineStatusText(final StatusEnum finalStatusEnum) {
    if (EventQueue.isDispatchThread()) {
      updateOnlineStatusTextOnSwingThread(finalStatusEnum);
    } else {
      SwingUtilities.invokeLater(new Runnable() {
        @Override
        public void run() {
          updateOnlineStatusTextOnSwingThread(finalStatusEnum);
        }
      });
    }
  }

  /**
   * Update online status text with new value.
   *
   * @param finalStatusEnum
   */
  public void updateOnlineStatusTextOnSwingThread(final StatusEnum finalStatusEnum) {
    this.statusEnum = finalStatusEnum;
    String onlineStatus = controller.getLocaliser().getString(finalStatusEnum.getLocalisationKey());
    if (finalStatusEnum == StatusEnum.ONLINE) {
      onlineLabel.setForeground(new Color(0, 100, 0));
      if (mainFrame != null) {
        BlinkLabel estimatedBalanceBTCLabel = mainFrame.getEstimatedBalanceBTCLabel();
        if (estimatedBalanceBTCLabel != null) {
          estimatedBalanceBTCLabel.setBlinkEnabled(true);
        }
        BlinkLabel estimatedBalanceFiatLabel = mainFrame.getEstimatedBalanceFiatLabel();
        if (estimatedBalanceFiatLabel != null) {
          estimatedBalanceFiatLabel.setBlinkEnabled(true);
        }
      }
    } else {
      onlineLabel.setForeground(new Color(180, 0, 0));
    }
    onlineLabel.setText(onlineStatus);
    if (finalStatusEnum == StatusEnum.ERROR) {
      // Set tooltip to look at Messages view
      String toolTip = HelpContentsPanel.createMultilineTooltipText(new String[]{
              controller.getLocaliser().getString("multiBitFrame.statusBar.error1"),
              controller.getLocaliser().getString("multiBitFrame.statusBar.error2")});
      onlineLabel.setToolTipText(toolTip);
    }
  }

  synchronized private void startSync() {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        // Switch off the Send button
        SendBitcoinPanel.setEnableSendButton(false);
        syncProgressBar.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        syncProgressBar.setValue(0);
        syncProgressBar.setVisible(true);


      }
    });
  }

  synchronized private void finishSync() {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        // Switch on the Send button
        SendBitcoinPanel.setEnableSendButton(true);
        syncProgressBar.setValue(100);
        syncProgressBar.setVisible(false);
      }
    });
  }

  synchronized private void updateSync(final int percent, final String syncMessage) {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        syncProgressBar.setValue(percent);
        syncProgressBar.setToolTipText(HelpContentsPanel.createTooltipText(syncMessage));

        // when a language changes the progress bar needs to be made visible
        syncProgressBar.setVisible(true);
      }
    });
  }

  @Override
  public void newMessageReceived(final Message newMessage) {
    if (newMessage == null || !newMessage.isShowInStatusBar()) {
      return;
    }

    if (newMessage.getPercentComplete() == Message.NOT_RELEVANT_PERCENTAGE_COMPLETE) {
      updateStatusLabel(newMessage.getText(), newMessage.isClearAutomatically());
    } else {
      if (Math.abs(newMessage.getPercentComplete() - 0) < TOLERANCE) {
        startSync();
      }
      updateSync((int) newMessage.getPercentComplete(), newMessage.getText());

      if (Math.abs(newMessage.getPercentComplete() - 100) < TOLERANCE) {
        finishSync();
      }
    }
  }

  private void updateStatusLabel(final String newStatusLabel, Boolean clearAutomatically) {
    StatusBar.clearAutomatically = clearAutomatically;
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        if (statusLabel != null) {
          log.debug("StatusBar label = '" + newStatusLabel + "'");
          statusLabel.setText(newStatusLabel);
        }
      }
    });
  }

  private void setZoneBorder(Border border) {
    zoneBorder = border;
  }

  /**
   * Adds a new zone in the StatusBar
   *
   * @param id
   * @param zone
   * @param constraints one of the constraint support by the
   *                    com.l2fprod.common.swing.PercentLayout
   */
  private void addZone(String id, Component zone, String constraints, String tweak) {
    // is there already a zone with this id?
    Component previousZone = getZone(id);
    if (previousZone != null) {
      remove(previousZone);
      idToZones.remove(id);
    }

    if (zone instanceof JComponent) {
      JComponent jc = (JComponent) zone;
      jc.setOpaque(true);
      //jc.setBackground(ColorAndFontConstants.BACKGROUND_COLOR);
      if (jc.getBorder() == null || jc.getBorder() instanceof UIResource) {
        if (jc instanceof JLabel) {
          if ("left".equals(tweak)) {
            Border border = new CompoundBorder(BorderFactory.createMatteBorder(0, 3, 0, 2, ColorAndFontConstants.BACKGROUND_COLOR), BorderFactory
                    .createLineBorder(Color.lightGray));
            jc.setBorder(border);
          } else {
            if ("right".equals(tweak)) {
              jc.setBorder(new CompoundBorder(zoneBorder, new EmptyBorder(0, 2, 0, 1)));
            } else {
              jc.setBorder(new CompoundBorder(zoneBorder, new EmptyBorder(0, 2, 0, 2)));
            }
          }
          ((JLabel) jc).setText(" ");
        } else {
          if (!(jc instanceof JPanel)) {
            jc.setBorder(zoneBorder);
          }
        }
      }
    }

    add(zone, constraints);
    idToZones.put(id, zone);
  }

  public Component getZone(String id) {
    return idToZones.get(id);
  }
}

/**
 * PercentLayout. <BR>
 * Constraint based layout which allow the space to be splitted using
 * percentages. The following are allowed when adding components to container:
 * <ul>
 * <li>container.add(component); <br>
 * in this case, the component will be sized to its preferred size
 * <li>container.add(component, "100"); <br>
 * in this case, the component will have a width (or height) of 100
 * <li>container.add(component, "25%"); <br>
 * in this case, the component will have a width (or height) of 25 % of the
 * container width (or height) <br>
 * <li>container.add(component, "*"); <br>
 * in this case, the component will take the remaining space. if several
 * components use the "*" constraint the space will be divided among the
 * components.
 * </ul>
 *
 * @javabean.class name="PercentLayout"
 * shortDescription="A layout supports constraints expressed in percent."
 */
class PercentLayout implements LayoutManager2 {

  /**
   * Useful constant to layout the components horizontally (from top to
   * bottom).
   */
  public final static int HORIZONTAL = 0;

  /**
   * Useful constant to layout the components vertically (from left to right).
   */
  public final static int VERTICAL = 1;

  static class Constraint {
    protected Object value;

    private Constraint(Object value) {
      this.value = value;
    }
  }

  static class NumberConstraint extends Constraint {
    public NumberConstraint(int d) {
      this(Integer.valueOf(d));
    }

    public NumberConstraint(Integer d) {
      super(d);
    }

    public int intValue() {
      return (Integer) value;
    }
  }

  static class PercentConstraint extends Constraint {
    public PercentConstraint(float d) {
      super(d);
    }

    public float floatValue() {
      return (Float) value;
    }
  }

  private final static Constraint REMAINING_SPACE = new Constraint("*");

  private final static Constraint PREFERRED_SIZE = new Constraint("");

  private int orientation;
  private int gap;

  private Locale locale;

  // Consider using HashMap
  private Hashtable<Component, Constraint> m_ComponentToConstraint;


  public PercentLayout(int orientation, int gap, Locale locale) {
    setOrientation(orientation);
    this.gap = gap;
    this.locale = locale;

    m_ComponentToConstraint = new Hashtable<Component, Constraint>();
  }

  public void setGap(int gap) {
    this.gap = gap;
  }

  /**
   * @javabean.property bound="true" preferred="true"
   */
  public int getGap() {
    return gap;
  }

  public void setOrientation(int orientation) {
    if (orientation != HORIZONTAL && orientation != VERTICAL) {
      throw new IllegalArgumentException("Orientation must be one of HORIZONTAL or VERTICAL");
    }
    this.orientation = orientation;
  }

  /**
   * @javabean.property bound="true" preferred="true"
   */
  public int getOrientation() {
    return orientation;
  }

  public void setConstraint(Component component, Object constraints) {
    if (constraints instanceof Constraint) {
      m_ComponentToConstraint.put(component, (Constraint) constraints);
    } else if (constraints instanceof Number) {
      setConstraint(component, new NumberConstraint(((Number) constraints).intValue()));
    } else if ("*".equals(constraints)) {
      setConstraint(component, REMAINING_SPACE);
    } else if ("".equals(constraints)) {
      setConstraint(component, PREFERRED_SIZE);
    } else if (constraints instanceof String) {
      String s = (String) constraints;
      if (s.endsWith("%")) {
        float value = Float.valueOf(s.substring(0, s.length() - 1)) / 100;
        if (value > 1 || value < 0) {
          throw new IllegalArgumentException("percent value must be >= 0 and <= 100");
        }
        setConstraint(component, new PercentConstraint(value));
      } else {
        setConstraint(component, new NumberConstraint(Integer.valueOf(s)));
      }
    } else if (constraints == null) {
      // null constraint means preferred size
      setConstraint(component, PREFERRED_SIZE);
    } else {
      throw new IllegalArgumentException("Invalid Constraint");
    }
  }

  @Override
  public void addLayoutComponent(Component component, Object constraints) {
    setConstraint(component, constraints);
  }

  /**
   * Returns the alignment along the x axis. This specifies how the component
   * would like to be aligned relative to other components. The value should
   * be a number between 0 and 1 where 0 represents alignment along the
   * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
   * etc.
   */
  @Override
  public float getLayoutAlignmentX(Container target) {
    return 1.0f / 2.0f;
  }

  /**
   * Returns the alignment along the y axis. This specifies how the component
   * would like to be aligned relative to other components. The value should
   * be a number between 0 and 1 where 0 represents alignment along the
   * origin, 1 is aligned the furthest away from the origin, 0.5 is centered,
   * etc.
   */
  @Override
  public float getLayoutAlignmentY(Container target) {
    return 1.0f / 2.0f;
  }

  /**
   * Invalidates the layout, indicating that if the layout manager has cached
   * information it should be discarded.
   */
  @Override
  public void invalidateLayout(Container target) {
  }

  /**
   * Adds the specified component with the specified name to the layout.
   *
   * @param name the component name
   * @param comp the component to be added
   */
  @Override
  public void addLayoutComponent(String name, Component comp) {
  }

  /**
   * Removes the specified component from the layout.
   *
   * @param comp the component ot be removed
   */
  @Override
  public void removeLayoutComponent(Component comp) {
    m_ComponentToConstraint.remove(comp);
  }

  /**
   * Calculates the minimum size dimensions for the specified panel given the
   * components in the specified parent container.
   *
   * @param parent the component to be laid out
   * @see #preferredLayoutSize
   */
  @Override
  public Dimension minimumLayoutSize(Container parent) {
    return preferredLayoutSize(parent);
  }

  /**
   * Returns the maximum size of this component.
   *
   * @see java.awt.Component#getMinimumSize()
   * @see java.awt.Component#getPreferredSize()
   * @see java.awt.LayoutManager
   */
  @Override
  public Dimension maximumLayoutSize(Container parent) {
    return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
  }

  @Override
  public Dimension preferredLayoutSize(Container parent) {
    Component[] components = parent.getComponents();
    Insets insets = parent.getInsets();
    int width = 0;
    int height = 0;
    Dimension componentPreferredSize;
    boolean firstVisibleComponent = true;
    for (int i = 0, c = components.length; i < c; i++) {
      if (components[i].isVisible()) {
        componentPreferredSize = components[i].getPreferredSize();
        if (orientation == HORIZONTAL) {
          height = Math.max(height, componentPreferredSize.height);
          width += componentPreferredSize.width;
          if (firstVisibleComponent) {
            firstVisibleComponent = false;
          } else {
            width += gap;
          }
        } else {
          height += componentPreferredSize.height;
          width = Math.max(width, componentPreferredSize.width);
          if (firstVisibleComponent) {
            firstVisibleComponent = false;
          } else {
            height += gap;
          }
        }
      }
    }
    return new Dimension(width + insets.right + insets.left, height + insets.top + insets.bottom);
  }

  @Override
  public void layoutContainer(Container parent) {
    Insets insets = parent.getInsets();
    Dimension d = parent.getSize();

    // calculate the available sizes
    d.width = d.width - insets.left - insets.right;
    d.height = d.height - insets.top - insets.bottom;

    // pre-calculate the size of each components
    Component[] components = parent.getComponents();
    int[] sizes = new int[components.length];

    // calculate the available size
    int availableSize = (HORIZONTAL == orientation ? d.width : d.height) - (components.length - 1) * gap;

    // PENDING(fred): the following code iterates 4 times on the component
    // array, need to find something more efficient!

    // give priority to components who want to use their preferred size or
    // who
    // have a predefined size
    for (int i = 0, c = components.length; i < c; i++) {
      if (components[i].isVisible()) {
        Constraint constraint = m_ComponentToConstraint.get(components[i]);
        if (constraint == null || constraint == PREFERRED_SIZE) {
          sizes[i] = (HORIZONTAL == orientation ? components[i].getPreferredSize().width : components[i]
                  .getPreferredSize().height);
          availableSize -= sizes[i];
        } else if (constraint instanceof NumberConstraint) {
          sizes[i] = ((NumberConstraint) constraint).intValue();
          availableSize -= sizes[i];
        }
      }
    }

    // then work with the components who want a percentage of the remaining
    // space
    int remainingSize = availableSize;
    for (int i = 0, c = components.length; i < c; i++) {
      if (components[i].isVisible()) {
        Constraint constraint = m_ComponentToConstraint.get(components[i]);
        if (constraint instanceof PercentConstraint) {
          sizes[i] = (int) (remainingSize * ((PercentConstraint) constraint).floatValue());
          availableSize -= sizes[i];
        }
      }
    }

    // finally share the remaining space between the other components
    ArrayList<Integer> remaining = new ArrayList<Integer>();
    for (int i = 0, c = components.length; i < c; i++) {
      if (components[i].isVisible()) {
        Constraint constraint = m_ComponentToConstraint.get(components[i]);
        if (constraint == REMAINING_SPACE) {
          remaining.add(i);
          sizes[i] = 0;
        }
      }
    }

    if (remaining.size() > 0) {
      int rest = availableSize / remaining.size();
      for (Integer aRemaining : remaining) {
        sizes[aRemaining] = rest;
      }
    }

    // all calculations are done, apply the sizes
    int currentOffset = (HORIZONTAL == orientation ? insets.left : insets.top);
    if (!ComponentOrientation.getOrientation(locale).isLeftToRight()) {
      currentOffset = (HORIZONTAL == orientation ? d.width - insets.right : insets.top);
    }

    for (int i = 0, c = components.length; i < c; i++) {
      if (components[i].isVisible()) {
        if (HORIZONTAL == orientation) {
          if (ComponentOrientation.getOrientation(locale).isLeftToRight()) {
            components[i].setBounds(currentOffset, insets.top, sizes[i], d.height);
          } else {
            components[i].setBounds(currentOffset - sizes[i], insets.top, sizes[i], d.height);
          }
        } else {
          components[i].setBounds(insets.left, currentOffset, d.width, sizes[i]);
        }
        if (ComponentOrientation.getOrientation(locale).isLeftToRight()) {
          currentOffset += gap + sizes[i];
        } else {
          currentOffset = currentOffset - gap - sizes[i];
        }
      }
    }
  }

}

class StatusClearTask extends TimerTask {
  JButton statusLabel;
  private String previousStatusLabelText = null;
  private int previousLabelRepeats = 0;

  StatusClearTask(JButton statusLabel) {
    this.statusLabel = statusLabel;
  }

  @Override
  public void run() {
    String currentStatusLabelText = statusLabel.getText();

    boolean hasReset = false;

    if (previousLabelRepeats > StatusBar.NUMBER_OF_REPEATS) {
      if (currentStatusLabelText != null && !"".equals(currentStatusLabelText)
              && currentStatusLabelText.equals(previousStatusLabelText)) {
        if (StatusBar.clearAutomatically) {
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              // clear label
              statusLabel.setText("");
            }
          });
          previousStatusLabelText = "";
          previousLabelRepeats = 0;
          hasReset = true;
        }
      }
    }
    if (currentStatusLabelText != null && !currentStatusLabelText.equals(previousStatusLabelText)) {
      // different - reset
      previousStatusLabelText = currentStatusLabelText;
      previousLabelRepeats = 0;
    } else {
      if (currentStatusLabelText != null && currentStatusLabelText.equals(previousStatusLabelText)) {
        if (!hasReset) {
          // label is the same as before
          previousLabelRepeats++;
        }
      }
    }
  }
}

/**
 * LookAndFeelTweaks
 */
final class LookAndFeelTweaks {

  public final static Border PANEL_BORDER = BorderFactory.createMatteBorder(3, 3, 3, 3, ColorAndFontConstants.MID_BACKGROUND_COLOR);

  /**
   * Utility class should not have a public constructor
   */
  private LookAndFeelTweaks() {
  }

  public static PercentLayout createHorizontalPercentLayout(Locale locale) {
    return new PercentLayout(PercentLayout.HORIZONTAL, 4, locale);
  }

  public static void setBorder(JComponent component) {
    if (component instanceof JPanel) {
      component.setBorder(PANEL_BORDER);
    }
  }
}