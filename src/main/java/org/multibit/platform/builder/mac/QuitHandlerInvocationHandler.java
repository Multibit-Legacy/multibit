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
package org.multibit.platform.builder.mac;

import org.multibit.platform.handler.GenericQuitHandler;
import org.multibit.platform.listener.GenericQuitEvent;
import org.multibit.platform.listener.GenericQuitResponse;

/**
 * <p>Implementation of invocation handler for Quit</p>
 */
public class QuitHandlerInvocationHandler extends BaseMacResponseInvocationHandler<GenericQuitHandler, GenericQuitEvent, GenericQuitResponse> {

    QuitHandlerInvocationHandler(GenericQuitHandler genericHandler, Class<GenericQuitEvent> genericEventClass, Class<GenericQuitResponse> genericResponseClass) {
        super(genericHandler, genericEventClass, genericResponseClass);
    }
}
