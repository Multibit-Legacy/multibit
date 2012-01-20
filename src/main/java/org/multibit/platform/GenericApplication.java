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
package org.multibit.platform;

/**
 * <p>Interface to provide the following to applications:</p>
 * <ul>
 * <li>Provision of simple platform identification methods</li>
 * </ul>
 *
 * @since 0.3.0
 *         
 */
public interface GenericApplication {

    /**
     * @return True if the application is running on a Mac AND the Apple Extensions API classes are available.
     */
    boolean isMac();

    /**
     * @return True if the application is running on a Mac AND the Linux Extensions API classes are available.
     */
    boolean isLinux();

    /**
     * @return True if the application is running on a Windows machine AND the Windows Extensions API classes are available.
     */
    boolean isWindows();



}
