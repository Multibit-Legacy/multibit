package org.multibit.model;


/**
 * an enum of the different sorts of data in the MultiBit system
 * this is mainly used as a guard as to the wrong type of data being sent to an action
 * @author jim
 *
 */
public enum DataType {
    ADDRESS,
    WALLET,
    PREFERENCES
}
