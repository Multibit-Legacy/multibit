/************************************************************
 * Copyright 2004-2005,2007-2008 Masahiko SAWAI All Rights Reserved. 
 * release under MIT license - same as MultiBit
 ************************************************************/
package org.multibit.viewsystem.swing.view.components;

import org.multibit.controller.Controller;
import org.multibit.viewsystem.swing.MultiBitFrame;
import org.multibit.viewsystem.swing.view.panels.HelpContentsPanel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import java.awt.*;
import java.awt.event.*;

/**
 * The <code>JFontChooser</code> class is a swing component for font selection.
 * This class has <code>JFileChooser</code> like APIs. The following code pops
 * up a font chooser dialog.
 * 
 * <pre>
 *   JFontChooser fontChooser = new JFontChooser();
 *   int result = fontChooser.showDialog(parent);
 *   if (result == JFontChooser.OK_OPTION)
 *   {
 *   	Font font = fontChooser.getSelectedFont(); 
 * }
 * 
 * <pre>
 **/
public class JFontChooser extends JComponent {
    
    private static final Logger log = LoggerFactory.getLogger(JFontChooser.class);

    private static final long serialVersionUID = 7081944796802758452L;

    private static final int WIDTH_DELTA = 240;
    private static final int HEIGHT_DELTA = 130;

    // class variables
    /**
     * Return value from <code>showDialog()</code>.
     * 
     * @see #showDialog
     **/
    public static final int OK_OPTION = 0;
    /**
     * Return value from <code>showDialog()</code>.
     * 
     * @see #showDialog
     **/
    public static final int CANCEL_OPTION = 1;
    /**
     * Return value from <code>showDialog()</code>.
     * 
     * @see #showDialog
     **/
    public static final int ERROR_OPTION = -1;
    private static final Font DEFAULT_SELECTED_FONT = new Font("Serif", Font.PLAIN, 13);
    private static Font defaultFont = new Font("Dialog", Font.PLAIN, 13);
    private static final int[] FONT_STYLE_CODES = { Font.PLAIN, Font.BOLD, Font.ITALIC, Font.BOLD | Font.ITALIC };
    private static final String[] DEFAULT_FONT_SIZE_STRINGS = { "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
            "21", "22", "23", "24" };

    // instance variables
    protected int dialogResultValue = ERROR_OPTION;

    private String[] fontStyleNames = null;
    private String[] fontFamilyNames = null;
    private String[] fontSizeStrings = null;
    private MultiBitTextField fontFamilyTextField = null;
    private MultiBitTextField fontStyleTextField = null;
    private MultiBitTextField fontSizeTextField = null;
    private JList fontNameList = null;
    private JList fontStyleList = null;
    private JList fontSizeList = null;
    private JPanel fontNamePanel = null;
    private JPanel fontStylePanel = null;
    private JPanel fontSizePanel = null;
    private JPanel samplePanel = null;
    private JTextField sampleText = null;

    private Controller controller;
    private FontMetrics fontMetrics;

    /**
     * Constructs a <code>JFontChooser</code> object.
     **/
    public JFontChooser(Controller controller) {
        this.controller = controller;
        this.fontSizeStrings = DEFAULT_FONT_SIZE_STRINGS;

        defaultFont = FontSizer.INSTANCE.getAdjustedDefaultFont();

        fontMetrics = getFontMetrics(defaultFont);

        int minimumWidth = fontMetrics.stringWidth(MultiBitFrame.EXAMPLE_LONG_FIELD_TEXT) + WIDTH_DELTA;
        int minimumHeight = 12 * fontMetrics.getHeight() + HEIGHT_DELTA;
        setMinimumSize(new Dimension(minimumWidth, minimumHeight));
        setPreferredSize(new Dimension(minimumWidth, minimumHeight));
        applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        JPanel selectPanel = new JPanel();
        selectPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        selectPanel.setLayout(new BoxLayout(selectPanel, BoxLayout.X_AXIS));
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            selectPanel.add(getFontFamilyPanel());
            selectPanel.add(getFontStylePanel());
            selectPanel.add(getFontSizePanel());
        } else {
            selectPanel.add(getFontSizePanel());            
            selectPanel.add(getFontStylePanel());
            selectPanel.add(getFontFamilyPanel());
        }

        JPanel contentsPanel = new JPanel();
        contentsPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        contentsPanel.setLayout(new GridLayout(2, 1));
        contentsPanel.add(selectPanel, BorderLayout.CENTER);
        contentsPanel.add(getSamplePanel(), BorderLayout.SOUTH);

        this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        this.add(contentsPanel);
        this.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        this.setSelectedFont(DEFAULT_SELECTED_FONT);
    }

    public JTextField getFontFamilyTextField() {
        if (fontFamilyTextField == null) {
            fontFamilyTextField = new MultiBitTextField("", 40, controller, 12);
            fontFamilyTextField.addFocusListener(new TextFieldFocusHandlerForTextSelection(fontFamilyTextField));
            fontFamilyTextField.addKeyListener(new TextFieldKeyHandlerForListSelectionUpDown(getFontFamilyList()));
            fontFamilyTextField.getDocument().addDocumentListener(new ListSearchTextFieldDocumentHandler(getFontFamilyList()));
            fontFamilyTextField.setFont(defaultFont);
            fontFamilyTextField.setEditable(false);
            fontFamilyTextField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            fontFamilyTextField.setMinimumSize(new Dimension(1, fontMetrics.getHeight() + 12));
        }
        return fontFamilyTextField;
    }

    public JTextField getFontStyleTextField() {
        if (fontStyleTextField == null) {
            fontStyleTextField = new MultiBitTextField("", 40, controller, 12);
            fontStyleTextField.addFocusListener(new TextFieldFocusHandlerForTextSelection(fontStyleTextField));
            fontStyleTextField.addKeyListener(new TextFieldKeyHandlerForListSelectionUpDown(getFontStyleList()));
            fontStyleTextField.getDocument().addDocumentListener(new ListSearchTextFieldDocumentHandler(getFontStyleList()));
            fontStyleTextField.setFont(defaultFont);
            fontStyleTextField.setEditable(false);
            fontStyleTextField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            fontStyleTextField.setMinimumSize(new Dimension(1, fontMetrics.getHeight() + 12));
        }
        return fontStyleTextField;
    }

    public JTextField getFontSizeTextField() {
        if (fontSizeTextField == null) {
            fontSizeTextField = new MultiBitTextField("", 40, controller, 12);
            fontSizeTextField.addFocusListener(new TextFieldFocusHandlerForTextSelection(fontSizeTextField));
            fontSizeTextField.addKeyListener(new TextFieldKeyHandlerForListSelectionUpDown(getFontSizeList()));
            fontSizeTextField.getDocument().addDocumentListener(new ListSearchTextFieldDocumentHandler(getFontSizeList()));
            fontSizeTextField.setFont(defaultFont);
            fontSizeTextField.setEditable(false);
            fontSizeTextField.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            fontSizeTextField.setMinimumSize(new Dimension(1, fontMetrics.getHeight() + 12));
        }
        return fontSizeTextField;
    }

    public JList getFontFamilyList() {
        if (fontNameList == null) {
            fontNameList = new JList(getFontFamilies());
            fontNameList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            fontNameList.addListSelectionListener(new ListSelectionHandler(getFontFamilyTextField()));
            fontNameList.setSelectedIndex(0);
            fontNameList.setFont(defaultFont);
            fontNameList.setFocusable(false);
            fontNameList.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        }
        return fontNameList;
    }

    public JList getFontStyleList() {
        if (fontStyleList == null) {
            fontStyleList = new JList(getFontStyleNames());
            fontStyleList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            fontStyleList.addListSelectionListener(new ListSelectionHandler(getFontStyleTextField()));
            fontStyleList.setSelectedIndex(0);
            fontStyleList.setFont(defaultFont);
            fontStyleList.setFocusable(false);
            fontStyleList.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }
        return fontStyleList;
    }

    public JList getFontSizeList() {
        if (fontSizeList == null) {
            fontSizeList = new JList(this.fontSizeStrings);
            fontSizeList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            fontSizeList.addListSelectionListener(new ListSelectionHandler(getFontSizeTextField()));
            fontSizeList.setSelectedIndex(0);
            fontSizeList.setFont(defaultFont);
            fontSizeList.setFocusable(false);
            fontSizeList.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }
        return fontSizeList;
    }

    /**
     * Get the family name of the selected font.
     * 
     * @return the font family of the selected font.
     * 
     * @see #setSelectedFontFamily
     **/
    public String getSelectedFontFamily() {
        String fontName = (String) getFontFamilyList().getSelectedValue();
        return fontName;
    }

    /**
     * Get the style of the selected font.
     * 
     * @return the style of the selected font. <code>Font.PLAIN</code>,
     *         <code>Font.BOLD</code>, <code>Font.ITALIC</code>,
     *         <code>Font.BOLD|Font.ITALIC</code>
     * 
     * @see java.awt.Font#PLAIN
     * @see java.awt.Font#BOLD
     * @see java.awt.Font#ITALIC
     * @see #setSelectedFontStyle
     **/
    public int getSelectedFontStyle() {
        int index = getFontStyleList().getSelectedIndex();
        return FONT_STYLE_CODES[index];
    }

    /**
     * Get the size of the selected font.
     * 
     * @return the size of the selected font
     * 
     * @see #setSelectedFontSize
     **/
    public int getSelectedFontSize() {
        int fontSize = 1;
        String fontSizeString = getFontSizeTextField().getText();
        log.debug("fontSize (A) = " + fontSizeString);
        try {
            fontSize = Integer.parseInt(fontSizeString);
        } catch (NumberFormatException e) {
            log.debug("fontSize (B) = " + fontSizeString);
            fontSizeString = (String) getFontSizeList().getSelectedValue();
            getFontSizeTextField().setText(fontSizeString);
        }

        return fontSize;
    }

    /**
     * Get the selected font.
     * 
     * @return the selected font
     * 
     * @see #setSelectedFont
     * @see java.awt.Font
     **/
    public Font getSelectedFont() {
        return new Font(getSelectedFontFamily(), getSelectedFontStyle(), getSelectedFontSize());
    }

    /**
     * Set the family name of the selected font.
     * 
     * @param name
     *            the family name of the selected font.
     * 
     **/
    public void setSelectedFontFamily(String name) {
        String[] names = getFontFamilies();
        for (int i = 0; i < names.length; i++) {
            if (names[i].equalsIgnoreCase(name.toLowerCase())) {
                getFontFamilyList().setSelectedIndex(i);
                break;
            }
        }
        updateSampleFont();
    }

    /**
     * Set the style of the selected font.
     * 
     * @param style
     *            the size of the selected font. <code>Font.PLAIN</code>,
     *            <code>Font.BOLD</code>, <code>Font.ITALIC</code>, or
     *            <code>Font.BOLD|Font.ITALIC</code>.
     * 
     * @see java.awt.Font#PLAIN
     * @see java.awt.Font#BOLD
     * @see java.awt.Font#ITALIC
     * @see #getSelectedFontStyle
     **/
    public void setSelectedFontStyle(int style) {
        for (int i = 0; i < FONT_STYLE_CODES.length; i++) {
            if (FONT_STYLE_CODES[i] == style) {
                getFontStyleList().setSelectedIndex(i);
                break;
            }
        }
        updateSampleFont();
    }

    /**
     * Set the size of the selected font.
     * 
     * @param size
     *            the size of the selected font
     * 
     * @see #getSelectedFontSize
     **/
    public void setSelectedFontSize(int size) {
        String sizeString = String.valueOf(size);
        for (int i = 0; i < this.fontSizeStrings.length; i++) {
            if (this.fontSizeStrings[i].equals(sizeString)) {
                getFontSizeList().setSelectedIndex(i);
                break;
            }
        }
        getFontSizeTextField().setText(sizeString);
        updateSampleFont();
    }

    /**
     * Set the selected font.
     * 
     * @param font
     *            the selected font
     * 
     * @see #getSelectedFont
     * @see java.awt.Font
     **/
    public void setSelectedFont(Font font) {
        setSelectedFontFamily(font.getFamily());
        setSelectedFontStyle(font.getStyle());
        setSelectedFontSize(font.getSize());
    }

    /**
     * Show font selection dialog.
     * 
     * @param parent
     *            Dialog's Parent component.
     * @return OK_OPTION, CANCEL_OPTION or ERROR_OPTION
     * 
     * @see #OK_OPTION
     * @see #CANCEL_OPTION
     * @see #ERROR_OPTION
     **/
    public int showDialog(Component parent) {
        dialogResultValue = ERROR_OPTION;
        JDialog dialog = createDialog(parent);
        dialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                dialogResultValue = CANCEL_OPTION;
            }
        });

        dialog.setVisible(true);
        dialog.dispose();
        dialog = null;

        return dialogResultValue;
    }

    protected class ListSelectionHandler implements ListSelectionListener {
        private JTextComponent textComponent;

        ListSelectionHandler(JTextComponent textComponent) {
            this.textComponent = textComponent;
        }

        @Override
        public void valueChanged(ListSelectionEvent e) {
            if (e.getValueIsAdjusting() == false) {
                JList list = (JList) e.getSource();
                String selectedValue = (String) list.getSelectedValue();

                String oldValue = textComponent.getText();
                textComponent.setText(selectedValue);
                if (!oldValue.equalsIgnoreCase(selectedValue)) {
                    textComponent.selectAll();
                    textComponent.requestFocus();
                }

                updateSampleFont();
            }
        }
    }

    protected class TextFieldFocusHandlerForTextSelection extends FocusAdapter {
        private JTextComponent textComponent;

        public TextFieldFocusHandlerForTextSelection(JTextComponent textComponent) {
            this.textComponent = textComponent;
        }

        @Override
        public void focusGained(FocusEvent e) {
            textComponent.selectAll();
        }

        @Override
        public void focusLost(FocusEvent e) {
            textComponent.select(0, 0);
            updateSampleFont();
        }
    }

    protected static class TextFieldKeyHandlerForListSelectionUpDown extends KeyAdapter {
        private JList targetList;

        public TextFieldKeyHandlerForListSelectionUpDown(JList list) {
            this.targetList = list;
        }

        @Override
        public void keyPressed(KeyEvent e) {
            int i = targetList.getSelectedIndex();
            switch (e.getKeyCode()) {
            case KeyEvent.VK_UP:
                i = targetList.getSelectedIndex() - 1;
                if (i < 0) {
                    i = 0;
                }
                targetList.setSelectedIndex(i);
                break;
            case KeyEvent.VK_DOWN:
                int listSize = targetList.getModel().getSize();
                i = targetList.getSelectedIndex() + 1;
                if (i >= listSize) {
                    i = listSize - 1;
                }
                targetList.setSelectedIndex(i);
                break;
            default:
                break;
            }
        }
    }

    protected static class ListSearchTextFieldDocumentHandler implements DocumentListener {
        JList targetList;

        public ListSearchTextFieldDocumentHandler(JList targetList) {
            this.targetList = targetList;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            update(e);
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            update(e);
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            update(e);
        }

        private void update(DocumentEvent event) {
            String newValue = "";
            try {
                Document doc = event.getDocument();
                newValue = doc.getText(0, doc.getLength());
            } catch (BadLocationException e) {
                e.printStackTrace();
            }

            if (newValue.length() > 0) {
                int index = targetList.getNextMatch(newValue, 0, Position.Bias.Forward);
                if (index < 0) {
                    index = 0;
                }
                targetList.ensureIndexIsVisible(index);

                String matchedName = targetList.getModel().getElementAt(index).toString();
                if (newValue.equalsIgnoreCase(matchedName)) {
                    if (index != targetList.getSelectedIndex()) {
                        SwingUtilities.invokeLater(new ListSelector(index));
                    }
                }
            }
        }

        public class ListSelector implements Runnable {
            private int index;

            public ListSelector(int index) {
                this.index = index;
            }

            @Override
            public void run() {
                targetList.setSelectedIndex(this.index);
            }
        }
    }

    protected class DialogOKAction extends AbstractAction {
        /**
         * 
         */
        private static final long serialVersionUID = 6849374420117448628L;
        protected static final String ACTION_NAME = "OK";
        private JDialog dialog;

        protected DialogOKAction(JDialog dialog) {
            this.dialog = dialog;
            putValue(Action.DEFAULT, ACTION_NAME);
            putValue(Action.ACTION_COMMAND_KEY, ACTION_NAME);
            putValue(Action.NAME, controller.getLocaliser().getString("fontChooser.ok"));
            putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("fontChooser.ok")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            dialogResultValue = OK_OPTION;
            dialog.setVisible(false);
        }
    }

    protected class DialogCancelAction extends AbstractAction {
        private static final long serialVersionUID = 7404160836867845484L;
        protected static final String ACTION_NAME = "Cancel";
        private JDialog dialog;

        protected DialogCancelAction(JDialog dialog) {
            this.dialog = dialog;
            putValue(Action.DEFAULT, ACTION_NAME);
            putValue(Action.ACTION_COMMAND_KEY, ACTION_NAME);
            putValue(Action.NAME, controller.getLocaliser().getString("fontChooser.cancel"));
            putValue(Action.SHORT_DESCRIPTION, HelpContentsPanel.createTooltipText(controller.getLocaliser().getString("fontChooser.cancel")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            dialogResultValue = CANCEL_OPTION;
            dialog.setVisible(false);
        }
    }

    protected JDialog createDialog(Component parent) {
        Frame frame = parent instanceof Frame ? (Frame) parent : (Frame) SwingUtilities.getAncestorOfClass(Frame.class, parent);
        JDialog dialog = new JDialog(frame, controller.getLocaliser().getString("fontChooser.selectFont"), true);

        dialog.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        
        Action okAction = new DialogOKAction(dialog);
        Action cancelAction = new DialogCancelAction(dialog);

        MultiBitButton okButton = new MultiBitButton(okAction, controller);
        okButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        
        MultiBitButton cancelButton = new MultiBitButton(cancelAction, controller);
        cancelButton.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

        JPanel buttonsPanel = new JPanel();
        buttonsPanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        buttonsPanel.setLayout(new GridLayout(2, 1));
        buttonsPanel.add(okButton);
        buttonsPanel.add(cancelButton);
        buttonsPanel.setBorder(BorderFactory.createEmptyBorder(25, 0, 10, 10));

        ActionMap actionMap = buttonsPanel.getActionMap();
        actionMap.put(cancelAction.getValue(Action.DEFAULT), cancelAction);
        actionMap.put(okAction.getValue(Action.DEFAULT), okAction);
        InputMap inputMap = buttonsPanel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        inputMap.put(KeyStroke.getKeyStroke("ESCAPE"), cancelAction.getValue(Action.DEFAULT));
        inputMap.put(KeyStroke.getKeyStroke("ENTER"), okAction.getValue(Action.DEFAULT));

        JPanel dialogTrailingPanel = new JPanel();
        dialogTrailingPanel.setLayout(new BorderLayout());
        dialogTrailingPanel.add(buttonsPanel, BorderLayout.NORTH);

        dialog.getContentPane().add(this, BorderLayout.CENTER);
        if (ComponentOrientation.LEFT_TO_RIGHT == ComponentOrientation.getOrientation(controller.getLocaliser().getLocale())) {
            dialog.getContentPane().add(dialogTrailingPanel, BorderLayout.EAST);
        } else {
            dialog.getContentPane().add(dialogTrailingPanel, BorderLayout.WEST);
            
        }
        dialog.pack();
        dialog.setLocationRelativeTo(frame);
        return dialog;
    }

    protected void updateSampleFont() {
        Font font = getSelectedFont();
        getSampleTextField().setFont(font);
    }

    protected JPanel getFontFamilyPanel() {
        if (fontNamePanel == null) {
            fontNamePanel = new JPanel();
            fontNamePanel.setLayout(new BorderLayout());
            fontNamePanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            //fontNamePanel.setPreferredSize(new Dimension(180, 130));
            fontNamePanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JScrollPane scrollPane = new JScrollPane(getFontFamilyList());
            scrollPane.getVerticalScrollBar().setFocusable(false);
            scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            scrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JPanel p = new JPanel();
            p.setLayout(new BorderLayout());
            p.add(getFontFamilyTextField(), BorderLayout.NORTH);
            p.add(scrollPane, BorderLayout.CENTER);

            MultiBitLabel label = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontName"));
            label.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            label.setHorizontalAlignment(JLabel.LEADING);
            label.setHorizontalTextPosition(JLabel.LEADING);
            label.setLabelFor(getFontFamilyTextField());
            label.setDisplayedMnemonic('F');

            fontNamePanel.add(label, BorderLayout.NORTH);
            fontNamePanel.add(p, BorderLayout.CENTER);

        }
        return fontNamePanel;
    }

    protected JPanel getFontStylePanel() {
        if (fontStylePanel == null) {
            fontStylePanel = new JPanel();
            fontStylePanel.setLayout(new BorderLayout());
            fontStylePanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            //fontStylePanel.setPreferredSize(new Dimension(140, 130));
            fontStylePanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JScrollPane scrollPane = new JScrollPane(getFontStyleList());
            scrollPane.getVerticalScrollBar().setFocusable(false);
            scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            scrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JPanel p = new JPanel();
            p.setLayout(new BorderLayout());
            p.add(getFontStyleTextField(), BorderLayout.NORTH);
            p.add(scrollPane, BorderLayout.CENTER);

            MultiBitLabel label = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontStyle"));
            label.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            label.setHorizontalAlignment(JLabel.LEADING);
            label.setHorizontalTextPosition(JLabel.LEADING);
            label.setLabelFor(getFontStyleTextField());
            label.setDisplayedMnemonic('Y');

            fontStylePanel.add(label, BorderLayout.NORTH);
            fontStylePanel.add(p, BorderLayout.CENTER);
        }
        return fontStylePanel;
    }

    protected JPanel getFontSizePanel() {
        if (fontSizePanel == null) {
            fontSizePanel = new JPanel();
            fontSizePanel.setLayout(new BorderLayout());
            //fontSizePanel.setPreferredSize(new Dimension(70, 130));
            fontSizePanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
            fontSizePanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JScrollPane scrollPane = new JScrollPane(getFontSizeList());
            scrollPane.getVerticalScrollBar().setFocusable(false);
            scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
            scrollPane.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            JPanel p = new JPanel();
            p.setLayout(new BorderLayout());
            p.add(getFontSizeTextField(), BorderLayout.NORTH);
            p.add(scrollPane, BorderLayout.CENTER);

            MultiBitLabel label = new MultiBitLabel(controller.getLocaliser().getString("fontChooser.fontSize"));
            label.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
            Dimension requiredSize = new Dimension(fontMetrics.stringWidth(controller.getLocaliser().getString(
                    "fontChooser.fontSize")), fontMetrics.getHeight());
            label.setMinimumSize(requiredSize);
            label.setMaximumSize(requiredSize);
            label.setPreferredSize(requiredSize);
            label.setHorizontalAlignment(JLabel.LEADING);
            label.setHorizontalTextPosition(JLabel.LEADING);
            label.setLabelFor(getFontSizeTextField());
            label.setDisplayedMnemonic('S');

            fontSizePanel.add(label, BorderLayout.NORTH);
            fontSizePanel.add(p, BorderLayout.CENTER);
        }
        return fontSizePanel;
    }

    protected JPanel getSamplePanel() {
        if (samplePanel == null) {
            TitledBorder titledBorder = BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), controller
                    .getLocaliser().getString("fontChooser.sampleText"));
            Border empty = BorderFactory.createEmptyBorder(5, 10, 10, 10);
            Border border = BorderFactory.createCompoundBorder(titledBorder, empty);

            titledBorder.setTitleFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
            samplePanel = new JPanel();
            samplePanel.setLayout(new BorderLayout());
            samplePanel.setBorder(border);
            samplePanel.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));

            samplePanel.add(getSampleTextField(), BorderLayout.CENTER);
        }
        return samplePanel;
    }

    protected JTextField getSampleTextField() {
        if (sampleText == null) {
            Border lowered = BorderFactory.createLoweredBevelBorder();

            sampleText = new JTextField(controller.getLocaliser().getString("fontChooser.sampleText"));
            sampleText.setBorder(lowered);
            sampleText.setPreferredSize(new Dimension(300, 100));
            sampleText.applyComponentOrientation(ComponentOrientation.getOrientation(controller.getLocaliser().getLocale()));
        }
        return sampleText;
    }

    protected String[] getFontFamilies() {
        if (fontFamilyNames == null) {
            GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
            fontFamilyNames = env.getAvailableFontFamilyNames();
        }
        return fontFamilyNames;
    }

    protected String[] getFontStyleNames() {
        if (fontStyleNames == null) {
            int i = 0;
            fontStyleNames = new String[4];
            fontStyleNames[i++] = controller.getLocaliser().getString("fontChooser.plain");
            fontStyleNames[i++] = controller.getLocaliser().getString("fontChooser.bold");
            fontStyleNames[i++] = controller.getLocaliser().getString("fontChooser.italic");
            fontStyleNames[i++] = controller.getLocaliser().getString("fontChooser.boldItalic");
        }
        return fontStyleNames;
    }
}
