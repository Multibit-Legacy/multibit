/**
 * Copyright 2012 multibit.org
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
package org.multibit.viewsystem.swing.action;

/**
 * <p>Exception to provide the following to {@link org.multibit.viewsystem.swing.action.CheckPrivateKeysSubmitAction}:</p>
 * <ul>
 * <li>Error handling for private key checking</li>
 * </ul>
 * <p>This base exception acts as a general failure mode not attributable to a specific cause (other than
 * that reported in the exception message). Since this is in English, it may not be worth reporting directly
 * to the user other than as part of a "general failure to parse" response.</p>
 *
 * @since 0.3.0
 */
public class PrivateKeysException extends RuntimeException {

    private static final long serialVersionUID = 2372470348971293437L;

    public PrivateKeysException(String s) {
        super(s);
    }

    public PrivateKeysException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
