package com.hishark.tech.jaas;

import java.io.IOException;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

public class SimpleCallbackHanlder implements javax.security.auth.callback.CallbackHandler{

	public void handle(Callback[] callbacks) throws IOException,
			UnsupportedCallbackException {
		for (int i = 0; i < callbacks.length; i++) {
			if(callbacks[i] instanceof NameCallback){
				((NameCallback)callbacks[i]).setName("fx50067");
			}else if(callbacks[i] instanceof PasswordCallback){
				((PasswordCallback)callbacks[i]).setPassword("Shark1011".toCharArray());
			}
		}
		
	}

}
