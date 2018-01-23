package com.hishark.tech.javaaccesscontroller;

import java.security.PrivilegedAction;

public class GetVMNamePrivilegedAction implements PrivilegedAction {

	public Object run() {
		String vmName = System.getProperty("java.vm.name");
		System.out.println("VM NAME in Privileged Code:" + vmName);
		return null;
	}

}
