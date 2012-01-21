package org.multibit.file;

import java.util.Date;

import com.google.bitcoin.core.ECKey;

/**
 * POFO containing an ECKey and Date
 * @author jim
 *
 */
public class PrivateKeyAndDate {
    private ECKey key;
    private Date date;
 
    public PrivateKeyAndDate() {
        
    }
    
    public PrivateKeyAndDate(ECKey key, Date date) {
        super();
        this.key = key;
        this.date = date;
    }
 
    @Override
    public String toString() {
        return "PrivateKeyAndDate [key=" + key + ", date=" + date + "]";
    }
    
    public ECKey getKey() {
        return key;
    }
    public void setKey(ECKey key) {
        this.key = key;
    }
    public Date getDate() {
        return date;
    }
    public void setDate(Date date) {
        this.date = date;
    }
    

}
