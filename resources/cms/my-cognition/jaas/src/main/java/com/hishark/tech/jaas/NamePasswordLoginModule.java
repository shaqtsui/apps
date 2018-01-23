package com.hishark.tech.jaas;

import java.util.Map;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.LoginException;

public class NamePasswordLoginModule implements
		javax.security.auth.spi.LoginModule {
	Subject arg0;
	CallbackHandler arg1;
	Map<String, ?> sharedState;
	Map<String, ?> options;

	NameCallback cbName;
	PasswordCallback cbPassword;
	SimplePrinciple principle;

	/**
	 * This will be invoked after all LoginModule verified failed
	 */
	public boolean abort() throws LoginException {
		// Remove stored answers
		cbName = null;
		cbPassword = null;
		return true;
	}

	/**
	 * This will be invoked after all LoginModule verified successfully
	 */

	public boolean commit() throws LoginException {
		// Add principle & credential(optional, if u want to use it later) to
		// subject (Any object can be
		// public/private credentials)
		SimplePrinciple principle = new SimplePrinciple(cbName.getName());
		arg0.getPrincipals().add(principle);
		arg0.getPrivateCredentials().add(new String(cbPassword.getPassword()));
		arg0.getPublicCredentials().add(cbName.getName());

		// store principle, will be used when logout
		this.principle = principle;

		return true;
	}


	public void initialize(Subject arg0, CallbackHandler arg1,
			Map<String, ?> sharedState, Map<String, ?> options) {
		this.arg0 = arg0;
		this.arg1 = arg1;
		this.sharedState = sharedState;
		this.options = options;
	}


	public boolean login() throws LoginException {
		boolean result = false;

		// Prepare Callback (we can take it as questions)
		NameCallback cbName = new NameCallback("Your Name:");
		PasswordCallback cbPassword = new PasswordCallback("Your Password:",
				false);
		Callback[] callbacks = new Callback[] { cbName, cbPassword };

		// Ask CallbackHandler to answer questions
		try {
			arg1.handle(callbacks);
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Get answer
		String name = cbName.getName();
		char[] password = cbPassword.getPassword();

		// Invoke Biz logic to verify answer
		if (name.equals("fx50067") && new String(password).equals("Shark1011")) {
			result = true;
		}

		// Save the answer, it will be used when commit() & logout been invoked
		if (result) {
			this.cbName = cbName;
			this.cbPassword = cbPassword;
		}

		return result;
	}


	public boolean logout() throws LoginException {
		// remove principle & credentials from subject
		arg0.getPrincipals().remove(principle);
		arg0.getPublicCredentials().remove(cbName.getName());
		arg0.getPrivateCredentials().remove(
				new String(cbPassword.getPassword()));
		return true;
	}

}
