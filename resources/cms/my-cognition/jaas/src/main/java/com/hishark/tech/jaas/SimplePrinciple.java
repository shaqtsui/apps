package com.hishark.tech.jaas;

import java.security.Principal;

public class SimplePrinciple implements Principal {
	String name;

	public SimplePrinciple(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

}
