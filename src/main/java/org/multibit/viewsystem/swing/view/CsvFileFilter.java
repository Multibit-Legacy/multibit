/**
 * Copyright 2013 multibit.org
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
package org.multibit.viewsystem.swing.view;

import java.io.File;

import org.multibit.controller.Controller;
import org.multibit.model.bitcoin.BitcoinModel;

public class CsvFileFilter extends javax.swing.filechooser.FileFilter {

    private Controller controller;
    
    public CsvFileFilter(Controller controller) {
        this.controller = controller;
    }
    
    @Override
    public boolean accept(File file) {
        return file.isDirectory() || (file.getName().toLowerCase().endsWith(BitcoinModel.CSV_FILE_EXTENSION));
    }

    @Override
    public String getDescription() {
        String descriptionText = controller.getLocaliser().getString("csvFileFilter.description");
        return descriptionText + " (*." + BitcoinModel.CSV_FILE_EXTENSION + ")";
    }
}
