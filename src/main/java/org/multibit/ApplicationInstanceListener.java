package org.multibit;

public interface ApplicationInstanceListener {
    /**
     * a new instance of MultiBit has been created with the command line argument of rawURI
     */
	public void newInstanceCreated(String rawURI);
}
