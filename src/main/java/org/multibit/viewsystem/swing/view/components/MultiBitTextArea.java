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
package org.multibit.viewsystem.swing.view.components;

import javax.swing.JTextArea;

import org.multibit.controller.MultiBitController;
import org.multibit.model.MultiBitModel;
import org.multibit.viewsystem.swing.MultiBitFrame;

public class MultiBitTextArea extends JTextArea {

    private static final long serialVersionUID = 3539740758937470378L;
   
    public MultiBitTextArea(String text, int height, int width, MultiBitController controller) {
        super(text, height, width);
        setFont(FontSizer.INSTANCE.getAdjustedDefaultFont());
    }
}
