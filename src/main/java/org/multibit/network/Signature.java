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
package org.multibit.network;

/** 
 * POJO Containing Signature information - sed by ParseResult
 */
public class Signature {
    private String publicKeyAsHex = null;
    private String signatureText = null;

    public String getPublicKeyAsHex() {
        return publicKeyAsHex;
    }

    public void setPublicKeyAsHex(String publicKeyAsHex) {
        this.publicKeyAsHex = publicKeyAsHex;
    }

    public String getSignatureText() {
        return signatureText;
    }

    public void setSignatureText(String signatureText) {
        this.signatureText = signatureText;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((publicKeyAsHex == null) ? 0 : publicKeyAsHex.hashCode());
        result = prime * result + ((signatureText == null) ? 0 : signatureText.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof Signature))
            return false;
        Signature other = (Signature) obj;
        if (publicKeyAsHex == null) {
            if (other.publicKeyAsHex != null)
                return false;
        } else if (!publicKeyAsHex.equals(other.publicKeyAsHex))
            return false;
        if (signatureText == null) {
            if (other.signatureText != null)
                return false;
        } else if (!signatureText.equals(other.signatureText))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "Signature [publicKeyAsHex=" + publicKeyAsHex + ", signatureText=" + signatureText + "]";
    }
}

