/**
 * Copyright 2012 Google Inc.
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
package org.multibit.store;

/**
 * <p>Exception to provide the following to {@link WalletInfo}:</p>
 * <ul>
 * <li>Provision of wallet version incorrect messages</li>
 * </ul>

 * @since 0.4.6
 */
public class WalletVersionException extends RuntimeException {

    private static final long serialVersionUID = 2379990341301293437L;

    public WalletVersionException(String s) {
        super(s);
    }

    public WalletVersionException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
