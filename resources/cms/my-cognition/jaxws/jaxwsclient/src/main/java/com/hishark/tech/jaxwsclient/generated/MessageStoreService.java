
package com.hishark.tech.jaxwsclient.generated;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.1.6 in JDK 6
 * Generated source version: 2.1
 * 
 */
@WebServiceClient(name = "MessageStoreService", targetNamespace = "http://server.jaxws.hishark.com/", wsdlLocation = "http://localhost/message?wsdl")
public class MessageStoreService
    extends Service
{

    private final static URL MESSAGESTORESERVICE_WSDL_LOCATION;
    private final static Logger logger = Logger.getLogger(com.hishark.tech.jaxwsclient.generated.MessageStoreService.class.getName());

    static {
        URL url = null;
        try {
            URL baseUrl;
            baseUrl = com.hishark.tech.jaxwsclient.generated.MessageStoreService.class.getResource(".");
            url = new URL(baseUrl, "http://localhost/message?wsdl");
        } catch (MalformedURLException e) {
            logger.warning("Failed to create URL for the wsdl Location: 'http://localhost/message?wsdl', retrying as a local file");
            logger.warning(e.getMessage());
        }
        MESSAGESTORESERVICE_WSDL_LOCATION = url;
    }

    public MessageStoreService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public MessageStoreService() {
        super(MESSAGESTORESERVICE_WSDL_LOCATION, new QName("http://server.jaxws.hishark.com/", "MessageStoreService"));
    }

    /**
     * 
     * @return
     *     returns MessageStore
     */
    @WebEndpoint(name = "MessageStorePort")
    public MessageStore getMessageStorePort() {
        return super.getPort(new QName("http://jaxwsserver.tech.hishark.com/", "MessageStorePort"), MessageStore.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns MessageStore
     */
    @WebEndpoint(name = "MessageStorePort")
    public MessageStore getMessageStorePort(WebServiceFeature... features) {
        return super.getPort(new QName("http://server.jaxws.hishark.com/", "MessageStorePort"), MessageStore.class, features);
    }

}
