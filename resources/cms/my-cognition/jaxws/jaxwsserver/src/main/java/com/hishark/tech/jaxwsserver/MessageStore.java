/**
 * 
 */
package com.hishark.tech.jaxwsserver;

import javax.jws.WebService;

@WebService
public class MessageStore {

	public String getMessage(String name) {
		// begin-user-code
		System.out.println("Request Comming....");
		return "Hello " + name;
		// end-user-code
	}
}