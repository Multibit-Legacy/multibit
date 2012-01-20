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
package org.multibit.platform.handler;

import org.multibit.platform.listener.GenericPreferencesEvent;

/**
 * <p>Generic handler to provide the following to {@link org.multibit.platform.GenericApplication}:</p>
 * <ul>
 * <li>Provision of application specific handling code</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericPreferencesHandler extends GenericHandler {
    /**
     * Called in response to receiving a Preferences event
     * @param event The generic Preferences event
     */
    void handlePreferences(GenericPreferencesEvent event);
}
