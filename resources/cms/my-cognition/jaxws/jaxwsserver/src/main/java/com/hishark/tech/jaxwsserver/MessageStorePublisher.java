package com.hishark.tech.jaxwsserver;

import javax.xml.ws.Endpoint;

public class MessageStorePublisher {
	public static void main(String[] args) {
		Endpoint.publish("http://localhost/message", new MessageStore());
	}
}
