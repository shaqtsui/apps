package com.hishark.tech.jaas;

import javax.security.auth.Subject;
import javax.security.auth.login.LoginContext;
import javax.security.auth.login.LoginException;

public class Authentication {

	/**
	 * @param args
	 * @throws LoginException
	 */
	public static void main(String[] args) throws LoginException {
		System.setProperty("java.security.auth.login.config",
				"src/com/hishark/jaas/jaas.authentication");

		// Prepare CallbackHandler (answer for callback)
		SimpleCallbackHanlder handler = new SimpleCallbackHanlder();
		
		// JAAS Authenticate start ...
		LoginContext context = new LoginContext("com.hishark.jaas", handler);
		context.login();
		Subject subject = context.getSubject();
		System.out.println(subject);
		context.logout();
		System.out.println(subject);
		

	}

}
