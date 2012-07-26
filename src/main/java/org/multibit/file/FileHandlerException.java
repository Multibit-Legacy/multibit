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


/**
 * Exception fo {@link FileHandler}.
 * 
 * @since 0.4.2
 */
public class FileHandlerException extends RuntimeException {

    private static final long serialVersionUID = 2372470997301293437L;

    public FileHandlerException(String s) {
        super(s);
    }

    public FileHandlerException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
