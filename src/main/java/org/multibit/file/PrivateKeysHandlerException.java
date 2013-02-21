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
package org.multibit.file;

import org.multibit.model.bitcoin.WalletInfoData;

/**
 * <p>Exception to provide the following to {@link WalletInfoData}:</p>
 * <ul>
 * <li>Provision of wallet info loading and saving messages</li>
 * </ul>
 * <p>This base exception acts as a general failure mode not attributable to a specific cause (other than
 * that reported in the exception message). Since this is in English, it may not be worth reporting directly
 * to the user other than as part of a "general failure to parse" response.</p>
 *
 * @since 0.3.0
 */
public class PrivateKeysHandlerException extends RuntimeException {

    private static final long serialVersionUID = 2372470341301293437L;

    public PrivateKeysHandlerException(String s) {
        super(s);
    }

    public PrivateKeysHandlerException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
