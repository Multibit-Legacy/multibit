package org.multibit.network;

import java.util.ArrayList;
import java.util.List;

import org.multibit.network.Signature;

/**
 * Class containing results of parsing the version.txt
 * version text stored on multibit.org
 */
public class ParseResult {
    private boolean newVersionIsAvailable = false;
    private String versionOnServer = null;
    private String localVersion = null;
    private List<String> messages = new ArrayList<String>();
    private List<Signature> signatures = new ArrayList<Signature>();
    
    public boolean isNewVersionIsAvailable() {
        return newVersionIsAvailable;
    }

    public void setNewVersionIsAvailable(boolean newVersionIsAvailable) {
        this.newVersionIsAvailable = newVersionIsAvailable;
    }

    public String getVersionOnServer() {
        return versionOnServer;
    }

    public void setVersionOnServer(String versionOnServer) {
        this.versionOnServer = versionOnServer;
    }

    public String getLocalVersion() {
        return localVersion;
    }

    public void setLocalVersion(String localVersion) {
        this.localVersion = localVersion;
    }

    public List<String> getMessages() {
        return messages;
    }

    public void setMessages(List<String> messages) {
        this.messages = messages;
    }

    public List<Signature> getSignatures() {
        return signatures;
    }

    public void setSignatures(List<Signature> signatures) {
        this.signatures = signatures;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((localVersion == null) ? 0 : localVersion.hashCode());
        result = prime * result + ((messages == null) ? 0 : messages.hashCode());
        result = prime * result + (newVersionIsAvailable ? 1231 : 1237);
        result = prime * result + ((signatures == null) ? 0 : signatures.hashCode());
        result = prime * result + ((versionOnServer == null) ? 0 : versionOnServer.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (!(obj instanceof ParseResult))
            return false;
        ParseResult other = (ParseResult) obj;
        if (localVersion == null) {
            if (other.localVersion != null)
                return false;
        } else if (!localVersion.equals(other.localVersion))
            return false;
        if (messages == null) {
            if (other.messages != null)
                return false;
        } else if (!messages.equals(other.messages))
            return false;
        if (newVersionIsAvailable != other.newVersionIsAvailable)
            return false;
        if (signatures == null) {
            if (other.signatures != null)
                return false;
        } else if (!signatures.equals(other.signatures))
            return false;
        if (versionOnServer == null) {
            if (other.versionOnServer != null)
                return false;
        } else if (!versionOnServer.equals(other.versionOnServer))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "ParseResult [newVersionIsAvailable=" + newVersionIsAvailable + ", versionOnServer=" + versionOnServer
                + ", localVersion=" + localVersion + ", messages=" + messages + ", signatures=" + signatures + "]";
    }
}
