package com.hishark.tech.jaxwsclient;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import com.hishark.tech.jaxwsclient.generated.MessageStore;
import com.hishark.tech.jaxwsclient.generated.MessageStoreService;

public class MessageStoreInvoker {
	public static void main(String[] args) throws MalformedURLException {
		//For server
		//String wsdlURL = "http://localhost:8080/jaxws/MessageStoreService?wsdl";
		//For Publisher
		String wsdlURL = "http://localhost/message?wsdl";
		
		MessageStoreService service = new MessageStoreService(new URL(
				wsdlURL), new QName(
				"http://jaxwsserver.tech.hishark.com/", "MessageStoreService"));
		MessageStore messageStore = service.getMessageStorePort();
		String res = messageStore.getMessage("Shark");
		System.out.println(res);
	}
}
