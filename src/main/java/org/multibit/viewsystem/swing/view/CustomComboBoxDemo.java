package org.multibit.viewsystem.swing.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;

import org.multibit.controller.MultiBitController;

/*
 * CustomComboBoxDemo.java is a 1.4 application that uses the following files:
 *   images/Bird.gif
 *   images/Cat.gif
 *   images/Dog.gif
 *   images/Rabbit.gif
 *   images/Pig.gif
 */
public class CustomComboBoxDemo extends JPanel {

    private static final long serialVersionUID = 1323L;
    ImageIcon[] images;
    String[] petStrings = { "Bird", "Cat", "Dog", "Rabbit", "Pig" };

    private Map<String, String> languageToLanguageCodeMap;
    private Map<String, String> languageCodeToLanguageMap;

    /*
     * Despite its use of EmptyBorder, this panel makes a fine content pane
     * because the empty border just increases the panel's size and is "painted"
     * on top of the panel's normal background. In other words, the JPanel fills
     * its entire background if it's opaque (which it is by default); adding a
     * border doesn't change that.
     */
    MultiBitController controller;

    public CustomComboBoxDemo(MultiBitController controller) {
        super(new BorderLayout());

        this.controller = controller;

        int numberOfLanguages = Integer.parseInt(controller.getLocaliser().getString("showPreferencesPanel.numberOfLanguages"));
        Integer[] intArray = new Integer[numberOfLanguages];
        for (int i = 1; i <= numberOfLanguages; i++) {
            String languageCode = controller.getLocaliser().getString("showPreferencesPanel.languageCode." + i);
            String language = controller.getLocaliser().getString("showPreferencesPanel.language." + i);
            languageToLanguageCodeMap.put(language, languageCode);
            languageCodeToLanguageMap.put(languageCode, language);

            intArray[i] = new Integer(i);
            images[i] = createImageIcon(languageCode);
            if (images[i] != null) {
                images[i].setDescription(petStrings[i]);
            }
        }

        // Create the combo box.
        JComboBox languageCodeList = new JComboBox(intArray);
        ComboBoxRenderer renderer = new ComboBoxRenderer();
        renderer.setPreferredSize(new Dimension(200, 130));
        languageCodeList.setRenderer(renderer);
        languageCodeList.setMaximumRowCount(3);

        // Lay out the demo.
        add(languageCodeList, BorderLayout.PAGE_START);
        setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
    }

    /** Returns an ImageIcon, or null if the path was invalid. */
    protected static ImageIcon createImageIcon(String path) {
        java.net.URL imgURL = CustomComboBoxDemo.class.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL);
        } else {
            System.err.println("Couldn't find file: " + path);
            return null;
        }
    }

    private ImageIcon createimageIcon(String text) {
        Font f = new Font("Serif", Font.BOLD, 12);
        Image image;

        JLabel textLabel = new JLabel(text);
        textLabel.setFont(f);

        MediaTracker mt = new MediaTracker(this);
        image = Toolkit.getDefaultToolkit().createImage("test.jpg");
        mt.addImage(image, 0);
        try {
            mt.waitForID(0);
        } catch (InterruptedException ie) {
        }
        int width = image.getWidth(null);
        int height = image.getHeight(null);
        BufferedImage bimg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        bimg.createGraphics().drawImage(image, 0, 0, this);
        bimg.getGraphics().setFont(f);
        bimg.getGraphics().drawString(text, 250, 100);

        return new ImageIcon(bimg);
    }

    class ComboBoxRenderer extends JLabel implements ListCellRenderer {
        private Font uhOhFont;

        public ComboBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(CENTER);
            setVerticalAlignment(CENTER);
        }

        /*
         * This method finds the image and text corresponding to the selected
         * value and returns the label, set up to display the text and image.
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                boolean cellHasFocus) {
            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }

            // Set the icon and text. If icon was null, say so.
            ImageIcon icon = images[selectedIndex];
            String pet = petStrings[selectedIndex];
            setIcon(icon);
            if (icon != null) {
                setText(pet);
                setFont(list.getFont());
            } else {
                setUhOhText(pet + " (no image available)", list.getFont());
            }

            return this;
        }

        // Set the font and text when no image was found.
        protected void setUhOhText(String uhOhText, Font normalFont) {
            if (uhOhFont == null) { // lazily create this font
                uhOhFont = normalFont.deriveFont(Font.ITALIC);
            }
            setFont(uhOhFont);
            setText(uhOhText);
        }
    }
}
