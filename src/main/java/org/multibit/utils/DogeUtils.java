package org.multibit.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Created by Max on 06.02.14.
 */
public class DogeUtils {

    private static Logger log = LoggerFactory.getLogger(DogeUtils.class);

    private static final URL DOGEPOOL_URL;
    private static final URL CRYPTSY_URL;
    private static final URL VIRCUREX_URL;

    static
    {
        try
        {
            DOGEPOOL_URL = new URL("http://dogepool.com/lastdoge");
            CRYPTSY_URL = new URL("http://pubapi.cryptsy.com/api.php?method=singlemarketdata&marketid=132");
            VIRCUREX_URL = new URL("https://vircurex.com/api/get_last_trade.json?base=DOGE&alt=BTC");
        }
        catch (final MalformedURLException x)
        {
            throw new RuntimeException(x); // cannot happen
        }
    }

    public static float requestDogeBtcConversion() {
        HttpURLConnection connection = null;
        Reader reader = null;

        try
        {
            connection = (HttpURLConnection) DOGEPOOL_URL.openConnection();
            connection.setConnectTimeout(Constants.HTTP_TIMEOUT_MS);
            connection.setReadTimeout(Constants.HTTP_TIMEOUT_MS);
            connection.connect();

            final int responseCode = connection.getResponseCode();
            if (responseCode == HttpURLConnection.HTTP_OK)
            {
                reader = new InputStreamReader(new BufferedInputStream(connection.getInputStream(), 1024), Constants.UTF_8);
                final StringBuilder content = new StringBuilder();
                Io.copy(reader, content);

                try
                {
                    return Float.parseFloat(content.toString());
                } catch (NumberFormatException e)
                {
                    log.debug("Hm, looks like dogepool changed their API...");
                    return -1;
                }

            }
            else
            {
                log.debug("http status " + responseCode + " when fetching " + DOGEPOOL_URL);
            }
        }
        catch (final Exception x)
        {
            log.debug("problem reading exchange rates", x);
        }
        finally
        {
            if (reader != null)
            {
                try
                {
                    reader.close();
                }
                catch (final IOException x)
                {
                    // swallow
                }
            }

            if (connection != null)
                connection.disconnect();
        }

        return -1;
    }
}
