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
package org.multibit.viewsystem.swing.view.ot;

import org.multibit.viewsystem.swing.view.*;
import java.util.HashMap;
import java.util.Map;

import org.multibit.controller.OTController;
import org.multibit.viewsystem.View;
import org.multibit.viewsystem.swing.MultiBitFrame;

/**
 * a factory class that lazy loads views
 * 
 * @author jim
 * 
 */
public class OTViewFactory {
    private Map<Integer, View> viewMap;

    OTController controller;
    MultiBitFrame mainFrame;

    public OTViewFactory(OTController controller, MultiBitFrame mainFrame) {
        this.controller = controller;
        this.mainFrame = mainFrame;
        initialise();
    }
    
    public void initialise() {
        viewMap = new HashMap<Integer, View>();        
    }

    public View getView(int viewNumber) {
        View viewToReturn = viewMap.get(viewNumber);

        if (viewToReturn == null) {
            viewToReturn = createView(viewNumber);
        }

        return viewToReturn;
    }

    public void addView(int viewNumber, View view) {
        viewMap.put(viewNumber, view);
    }
    
    private View createView(int viewNumber) {
        View viewToReturn = null;

        switch (viewNumber) {

        case View.SAME_VIEW: {
            assert false;
            break;
        }

        default: {
        }
        }

        if (viewToReturn != null) {
            viewMap.put(viewNumber, viewToReturn);
        }
        return viewToReturn;
    }
}
